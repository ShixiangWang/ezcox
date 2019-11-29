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
#' @param keep_models If `TRUE`, keep models as local files.
#' @param return_models default `FALSE`. If `TRUE`, return a `list` contains
#' cox models.
#' @param model_dir a path for storing model results.
#' @param parallel if `TRUE`, do parallel computation by **furrr** package.
#' @param verbose if `TRUE`, print extra info. If `parallel` is `TRUE`,
#' set `verbose` to `FALSE` may speed up.
#' @import survival
#' @importFrom stats as.formula
#' @importFrom dplyr tibble
#' @importFrom purrr map2_df
#' @return a `ezcox` object
#' @author Shixiang Wang <w_shixiang@163.com>
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
#' \donttest{
#' # Set verbose=FALSE
#' # may speed up when use parallel computation
#' # parallel=TRUE is not recommended for
#' # number of variables less than 100
#' ezcox(lung, covariates = c("sex", "ph.ecog"), controls = "age", parallel = TRUE, verbose = FALSE)
#' }
#'
#' # Return models
#' ezcox(lung,
#'   covariates = c("age", "sex", "ph.ecog"),
#'   return_models = TRUE
#' )
#' ezcox(lung,
#'   covariates = c("sex", "ph.ecog"), controls = "age",
#'   return_models = TRUE
#' )
ezcox <- function(data, covariates, controls = NULL,
                  time = "time", status = "status",
                  global_method = c("likelihood", "wald", "logrank"),
                  keep_models = FALSE,
                  return_models = FALSE,
                  model_dir = file.path(tempdir(), "ezcox"),
                  parallel = FALSE,
                  verbose = TRUE) {
  if (!"survival" %in% .packages()) {
    loadNamespace("survival")
  }

  if (!dir.exists(model_dir)) {
    dir.create(model_dir, recursive = TRUE)
  }

  data$time <- data[[time]]
  data$status <- data[[status]]

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
    model_df = dplyr::tibble(
      Variable = covariates,
      control = ifelse(exists("controls2"),
                         paste(controls2, collapse = ","),
                         NA_character_
      )
    )
  }

  batch_one <- function(x, y, controls = NULL, return_models = FALSE, verbose = TRUE) {
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
      cox <- tryCatch(coxph(fm, data = data),
        error = function(e) {
          if (verbose) {
            message("==> Something wrong with variable ", y)
            message("====> ", e)
          }
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
      if (verbose) message("==> Variable ", y, "has less than 2 levels, skipping it...")
      tbl <- dplyr::tibble(
        contrast_level = NA,
        ref_level = NA,
        n_contrast = NA,
        n_ref = NA
      )
      cox <- NA
    }


    if (return_models) {
      model_file = tempfile(pattern = "ezcox_", tmpdir = model_dir)
      model_df = dplyr::tibble(
        Variable = y,
        model = list(cox),
        status = ifelse(class(cox) == "coxph", TRUE, FALSE)
      )
      saveRDS(model_df, file = model_file)
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
    if (verbose) message("==> Done.")
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
      global.pval = glob.pval,
      model_file = ifelse(exists("model_file"), model_file, NA_character_)
    )
  }

  if (parallel) {
    if (length(covariates2) < 50) {
      if (verbose) message("Warning: variable < 50, parallel option is not recommended!")
    }

    if (!requireNamespace("furrr")) {
      stop("Please install 'furrr' package firstly!")
    }

    oplan <- future::plan()
    future::plan("multiprocess")
    on.exit(future::plan(oplan), add = TRUE)
    res <- furrr::future_map2_dfr(covariates2, covariates, batch_one,
                                  controls = controls,
                                  return_models = return_models | keep_models,
                                  verbose = verbose,
                                  .progress = TRUE
    )

  } else {
    res <- purrr::map2_df(covariates2, covariates, batch_one,
      controls = controls,
      return_models = return_models | keep_models,
      verbose = verbose
    )
  }

  if (return_models | keep_models) {

    models = dplyr::left_join(
      model_df,
      res %>%
        dplyr::select(c("Variable", "model_file")) %>%
        unique(),
      by = "Variable"
    )

    if (return_models) {
      if (parallel) {
        model_df = furrr::future_map_dfr(models$model_file, function(x) {
          readRDS(x)
        }, .progress = TRUE)
      } else {
        model_df = purrr::map_df(models$model_file, function(x) {
          readRDS(x)
        })
      }

      models = dplyr::left_join(models, model_df, by = "Variable")
    }

    res <- list(
      res = res,
      models = models
    )
  }

  res$res$model_file = NULL
  class(res) <- c("ezcox", class(res))
  attr(res, "controls") <- controls
  res
}
