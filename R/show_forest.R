#' Show Forest Plot
#'
#' This is a wrapper of function [ezcox], [get_models] and
#' [show_models]. If focus on generating forest plot easily
#' and flexibly.
#'
#' @inheritParams ezcox
#' @inheritParams show_models
#' @param vars_to_show default is `NULL`, show all variables
#' (including controls). You can use this to choose variables
#' to show, but remember, the models have not been changed.
#'
#' @return a `ggplot` object
#' @export
#'
#' @examples
#' library(survival)
#' show_forest(lung, covariates = c("sex", "ph.ecog"), controls = "age")
#' show_forest(lung, covariates = c("sex", "ph.ecog"), controls = "age", merge_models = TRUE)
#' show_forest(lung,
#'   covariates = c("sex", "ph.ecog"), controls = "age", merge_models = TRUE,
#'   drop_controls = TRUE
#' )
#' show_forest(lung,
#'   covariates = c("sex", "ph.ecog"), controls = "age", merge_models = TRUE,
#'   vars_to_show = "sex"
#' )
show_forest <- function(data, covariates, controls = NULL,
                        time = "time",
                        status = "status",
                        merge_models = FALSE,
                        model_names = NULL,
                        vars_to_show = NULL,
                        drop_controls = FALSE,
                        model_dir = file.path(tempdir(), "ezcox"),
                        ...) {
  fit_df <- ezcox(data,
    covariates = covariates,
    controls = controls,
    time = time, status = status,
    model_dir = model_dir,
    return_models = TRUE
  )
  fit_models <- get_models(fit_df)
  show_models(fit_models,
    model_names = model_names,
    covariates = vars_to_show,
    merge_models = merge_models,
    drop_controls = drop_controls,
    ...
  )
}
