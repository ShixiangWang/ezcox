#' Run Cox Analysis in Batch Mode
#'
#' @param data a `data.frame` containing variables, time and os status.
#' @param covariates column names specifying variables.
#' @param controls column names specifying controls.
#' The names with pattern "*:|()" will be treated as interaction/combination
#' term, please make sure all column names in `data` are valid R variable names.
#' @param time column name specifying time, default is 'time'.
#' @param status column name specifying event status, default is 'status'.
#' @param global_method method used to obtain global p value for cox model,
#' should be one of "likelihood", "wald", "logrank".
#' The likelihood-ratio test, Wald test, and score logrank statistics.
#' These three methods are asymptotically equivalent. For large enough N,
#' they will give similar results. For small N, they may differ somewhat.
#' The Likelihood ratio test has better behavior for small sample sizes,
#' so it is generally preferred.
#' @param keep_models If `TRUE`, keep models as local files.
#' @param return_models default `FALSE`. If `TRUE`, return a `list` contains
#' cox models.
#' @param model_dir a path for storing model results.
#' @param verbose if `TRUE`, print extra info.
#' @param ... other parameters passing to [survival::coxph()].
#' @import utils
#' @import survival
#' @importFrom stats as.formula
#' @importFrom dplyr tibble
#' @importFrom purrr map2_df
#' @importFrom methods is
#' @importFrom utf8 output_utf8
#' @return a `ezcox` object
#' @author Shixiang Wang <w_shixiang@163.com>
#' @export
#'
#' @examples
#' library(survival)
#'
#' # Build unvariable models
#' t1 <- ezcox(lung, covariates = c("age", "sex", "ph.ecog"))
#' t1
#'
#' # Build multi-variable models
#' # Control variable 'age'
#' t2 <- ezcox(lung, covariates = c("sex", "ph.ecog"), controls = "age")
#' t2
#'
#' # Return models
#' t3 <- ezcox(lung,
#'   covariates = c("age", "sex", "ph.ecog"),
#'   return_models = TRUE
#' )
#' t3
#' t4 <- ezcox(lung,
#'   covariates = c("sex", "ph.ecog"), controls = "age",
#'   return_models = TRUE
#' )
#' t4
#' @testexamples
#' expect_s3_class(t1, "ezcox")
#' expect_s3_class(t2, "ezcox")
#' expect_s3_class(t3, "ezcox")
#' expect_s3_class(t4, "ezcox")
ezcox <- function(data, covariates, controls = NULL,
                  time = "time", status = "status",
                  global_method = c("likelihood", "wald", "logrank"),
                  keep_models = FALSE,
                  return_models = FALSE,
                  model_dir = file.path(tempdir(), "ezcox"),
                  verbose = TRUE, ...) {
  stopifnot(is.data.frame(data))

  if (!"survival" %in% .packages()) {
    loadNamespace("survival")
  }

  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }

  data$time <- data[[time]]
  data$status <- data[[status]]
  data <- dplyr::filter(data, !is.na(.data$time), !is.na(.data$status))

  test_method <- match.arg(global_method)
  test_method <- switch(test_method,
    likelihood = test <- "logtest",
    wald = test <- "waldtest",
    logrank = test <- "sctest"
  )

  covariates2 <- ifelse(isValidAndUnreserved(covariates), covariates, paste0("`", covariates, "`"))
  if (!is.null(controls)) {
    controls2 <- controls
    controls <- ifelse(isValidAndUnreserved(controls), controls, paste0("`", controls, "`"))
  }

  if (return_models | keep_models) {
    model_df <- dplyr::tibble(
      Variable = covariates,
      control = ifelse(exists("controls2"),
        paste(controls2, collapse = ","),
        NA_character_
      )
    )
  }

  batch_one <- function(x, y, controls = NULL, return_models = FALSE, verbose = TRUE, ...) {
    if (!is.null(controls)) {
      type <- "multi"
    } else {
      type <- "single"
    }

    if (verbose) message("=> Processing variable ", y)

    if (length(table(data[[y]])) > 1) {
      if (verbose) message("==> Building Surv object...")
      fm <- as.formula(paste(
        "Surv(time, status)~", x,
        ifelse(type == "multi", paste0("+", paste(controls, collapse = "+")), "")
      ))
      if (verbose) message("==> Building Cox model...")
      cox <- tryCatch(coxph(fm, data = data, ...),
        error = function(e) {
          if (verbose) {
            message("==> Something wrong with variable ", y)
            message("====> ", e)
          }
        }
      )

      tbl <- purrr::map_df(c(y, gsub("`", "", controls)), function(x) {
        if (is.numeric(data[[x]])) {
          dplyr::tibble(
            contrast_level = x,
            ref_level = x,
            n_contrast = sum(!is.na(data[[x]])),
            n_ref = sum(!is.na(data[[x]]))
          )
        } else if (is.null(data[[x]])) {
          message("==> Handling combination term, some columns will be set to NA")
          dplyr::tibble(
            contrast_level = NA,
            ref_level = NA,
            n_contrast = NA,
            n_ref = NA
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
      if (verbose) message("==> Variable ", y, "has less than 2 levels, skipping it...")
      tbl <- dplyr::tibble(
        contrast_level = NA,
        ref_level = NA,
        n_contrast = NA,
        n_ref = NA
      )
      cox <- NA
    }

    if (is.numeric(data[[y]])) {
      n_var <- 1
    } else {
      n_var <- length(table(data[[y]])) - 1
    }

    tbl$is_control <- c(rep(FALSE, n_var), rep(TRUE, nrow(tbl) - n_var))


    if (return_models) {
      model_file <- tempfile(pattern = "ezcox_", tmpdir = model_dir)
      model_df <- dplyr::tibble(
        Variable = y,
        model = list(cox),
        status = ifelse(is(cox, "coxph"), TRUE, FALSE)
      )
      saveRDS(model_df, file = model_file)
    }

    if (any(grepl("[*:|()]", controls)) & is(cox, "coxph")) {
      out <- as.data.frame(summary(cox)$coef)
      cox_sum <- summary(cox)
      glob.pval <- signif(cox_sum[[test]]["pvalue"], digits = 3)

      dplyr::tibble(
        Variable = y,
        is_control = y != rownames(out),
        contrast_level = rownames(out),
        ref_level = NA,
        n_contrast = NA,
        n_ref = NA,
        beta = out[, 1],
        HR = out[, 2],
        lower_95 = tryCatch(signif(cox_sum$conf.int[, "lower .95"], 3),
          error = function(e) NA
        ),
        upper_95 = tryCatch(signif(cox_sum$conf.int[, "upper .95"], 3),
          error = function(e) NA
        ),
        p.value = out[, 5],
        global.pval = glob.pval,
        model_file = ifelse(exists("model_file"), model_file, NA_character_)
      )
    } else {
      if (!is(cox, "coxph") | all(is.na(tbl[["ref_level"]]))) {
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
      if (verbose) message("==> Done.")
      dplyr::tibble(
        Variable = y,
        is_control = tbl[["is_control"]],
        contrast_level = tbl[["contrast_level"]],
        ref_level = tbl[["ref_level"]],
        n_contrast = tbl[["n_contrast"]],
        n_ref = tbl[["n_ref"]],
        beta = beta,
        HR = HR,
        lower_95 = lower_95,
        upper_95 = upper_95,
        p.value = p.value,
        global.pval = glob.pval,
        model_file = ifelse(exists("model_file"), model_file, NA_character_)
      )
    }
  }


  res <- purrr::map2_df(covariates2, covariates, batch_one,
    controls = controls,
    return_models = return_models | keep_models,
    verbose = verbose,
    ...
  )

  if (return_models | keep_models) {
    models <- dplyr::left_join(
      model_df,
      res %>%
        dplyr::select(c("Variable", "model_file")) %>%
        unique(),
      by = "Variable"
    )

    if (return_models) {
      model_df <- purrr::map_df(models$model_file, function(x) {
        readRDS(x)
      })

      models <- dplyr::left_join(models, model_df, by = "Variable")
    }

    res$model_file <- NULL
    res <- list(
      res = res,
      models = models
    )
  } else {
    res$model_file <- NULL
  }

  class(res) <- c("ezcox", class(res))
  attr(res, "controls") <- gsub("`", "", controls)
  res
}
