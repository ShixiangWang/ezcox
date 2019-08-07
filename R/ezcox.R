#' Run Cox Analysis in Batch Mode
#'
#' @param data a `data.frame`.
#' @param covariates column names specifying variables.
#' @param controls column names specifying controls.
#' @param time column name specifying time, default is 'time'.
#' @param status column name specifying event status, default is 'status'.
#' @param global_method method used to obtain global p value for cox model,
#' should be one of "likelihood", "wald", "logrank".
#' The likelihood-ratio test, Wald test, and score logrank statistics.
#' These three methods are asymptotically equivalent. For large enough N,
#' they will give similar results. For small N, they may differ somewhat.
#' The Likelihood ratio test has better behavior for small sample sizes,
#' so it is generally preferred.
#' @param return_models default `FALSE`. If `TRUE`, return a `list` contains
#' cox models.
#' @import survival
#' @importFrom stats as.formula
#' @importFrom dplyr tibble
#' @importFrom purrr map2_df
#' @return a `tibble` or a `list`
#' @export
#'
#' @examples
#' library(survival)
#'
#' # Build unvariable models
#' ezcox(lung, covariates = c("age", "sex", "ph.ecog"))
#'
#' # Build multi-variable models
#' # Control variable 'age'
#' ezcox(lung, covariates = c("sex", "ph.ecog"), controls = "age")
#'
#' # Return models
#' ezcox(lung, covariates = c("age", "sex", "ph.ecog"),
#'             return_models = TRUE)
#' ezcox(lung, covariates = c("sex", "ph.ecog"), controls = "age",
#'             return_models = TRUE)
ezcox <- function(data, covariates, controls = NULL,
                  time = "time", status = "status",
                  global_method = c("likelihood", "wald", "logrank"),
                  return_models = FALSE) {
  if (!"survival" %in% .packages()) {
    loadNamespace("survival")
  }

  data$time <- data[[time]]
  data$status <- data[[status]]

  test_method <- match.arg(global_method)
  test_method <- switch(test_method,
    likelihood = test <- "logtest",
    wald = test <- "waldtest",
    logrank = test <- "sctest"
  )

  # https://stackoverflow.com/questions/8396577/check-if-character-value-is-a-valid-r-object-name
  isValidAndUnreserved <- function(string) {
    make.names(string) == string
  }
  covariates2 <- ifelse(isValidAndUnreserved(covariates), covariates, paste0("`", covariates, "`"))
  if (!is.null(controls)) {
    controls2 <- controls
    controls <- ifelse(isValidAndUnreserved(controls), controls, paste0("`", controls, "`"))
  }

  if (return_models) {
    model_env = new.env(parent = emptyenv())
    model_env$Variable = covariates
    model_env$controls = ifelse(exists("controls2"),
                                paste(controls2, collapse = ","),
                                NA_character_)
    model_env$models = list()
    model_env$status = logical()
  }

  batch_one <- function(x, y, controls = NULL, return_models = FALSE) {
    if (!is.null(controls)) {
      type <- "multi"
    } else {
      type <- "single"
    }
    message("=> Processing variable ", y)

    if (length(table(data[[y]])) > 1) {
      message("==> Building Surv object...")
      fm <- as.formula(paste(
        "Surv(time, status)~", x,
        ifelse(type == "multi", paste0("+", paste(controls, collapse = "+")), "")
      ))
      message("==> Building Cox model...")
      cox <- tryCatch(coxph(fm, data = data),
        error = function(e) {
          message("==> Something wrong with variable ", y)
          message("====> ", e)
        }
      )

      tbl <- purrr::map_df(c(y, controls), function(x) {
        if (is.numeric(data[[x]])) {
          dplyr::tibble(
            contrast_level = x,
            ref_level = x,
            n_contrast = sum(!is.na(data[[x]])),
            n_ref = sum(!is.na(data[[x]]))
          )
        } else {
          dplyr::tibble(
            contrast_level = names(table(data[[x]]))[-1],
            ref_level = names(table(data[[x]]))[1],
            n_contrast = as.numeric(table(data[[x]]))[-1],
            n_ref = as.numeric(table(data[[x]]))[1]
          )
        }
      })
    } else {
      message("==> Variable ", y, "has less than 2 levels, skipping it...")
      tbl <- dplyr::tibble(
        contrast_level = NA,
        ref_level = NA,
        n_contrast = NA,
        n_ref = NA
      )
      cox <- NA
    }


    if (return_models) {
      model_env$models[[length(model_env$models) + 1]] = cox
      model_env$status %<>% append(ifelse(class(cox) == "coxph", TRUE, FALSE))
    }

    if (class(cox) != "coxph" | all(is.na(tbl[["ref_level"]]))) {
      glob.pval <- p.value <- beta <- HR <- lower_95 <- upper_95 <- NA
    } else {
      cox <- summary(cox)
      p.value <- cox$coefficients[, "Pr(>|z|)"] %>%
        signif(digits = 3) %>%
        as.numeric()
      glob.pval <- signif(cox[[test]]["pvalue"], digits = 3)

      beta <- signif(cox$coefficients[, 1], digits = 3)
      HR <- signif(cox$coefficients[, 2], digits = 3)
      lower_95 <- tryCatch(signif(cox$conf.int[, "lower .95"], 3),
        error = function(e) NA
      )
      upper_95 <- tryCatch(signif(cox$conf.int[, "upper .95"], 3),
        error = function(e) NA
      )
    }
    message("==> Done.")
    dplyr::tibble(
      Variable = y,
      contrast_level = tbl[["contrast_level"]],
      ref_level = tbl[["ref_level"]],
      n_contrast = tbl[["n_contrast"]],
      n_ref = tbl[["n_ref"]],
      beta = beta,
      HR = HR,
      lower_95 = lower_95,
      upper_95 = upper_95,
      p.value = p.value,
      global.pval = glob.pval
    )
  }

  res = purrr::map2_df(covariates2, covariates, batch_one, controls = controls,
                 return_models = return_models)

  if (return_models) {
    models = dplyr::tibble(
      Variable = model_env$Variable,
      control = model_env$controls,
      model = model_env$models,
      status = model_env$status
    )
    res = list(res = res,
         models = models)
    class(res) = "ezcox"
  }

  res
}
