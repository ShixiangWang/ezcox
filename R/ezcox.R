#' Run Cox Analysis in Batch Mode
#'
#' We build model for each covariate with the following formula,
#' the controls are optional.
#'
#' coxph(Surv(time, status) ~ covariate + controls)
#'
#' @param data a `data.frame` containing variables, time and os status.
#' @param covariates column names specifying variables.
#' @param controls column names specifying controls.
#' @param time column name specifying time, default is 'time'.
#' @param status column name specifying event status, default is 'status'.
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

      tbl <- dplyr::bind_cols(
        dplyr::tibble(
          Variable = y,
        ),
        broom::tidy(
          cox,
          exponentiate = TRUE, conf.int = TRUE,
          conf.level = 0.95
        ) %>% dplyr::rename(
          HR = .data$estimate
        ),
        broom::glance(cox) %>%
          dplyr::select(c(
            "n", "nevent", "p.value.log", "p.value.sc", "p.value.wald",
            "r.squared", "AIC", "BIC"
          ))
      )
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
        status = ifelse(class(cox) == "coxph", TRUE, FALSE)
      )
      saveRDS(model_df, file = model_file)
    }

    tbl$model_file <- ifelse(exists("model_file"), model_file, NA_character_)

    if (verbose) message("==> Done.")
    tbl
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
