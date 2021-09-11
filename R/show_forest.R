#' Show Forest Plot
#'
#' This is a wrapper of function [ezcox], [get_models] and
#' [show_models]. It focus on generating forest plot easily
#' and flexibly.
#'
#' @inheritParams show_models
#' @inheritParams ezcox
#' @param vars_to_show default is `NULL`, show all variables
#' (including controls). You can use this to choose variables
#' to show, but remember, the models have not been changed.
#' @param add_caption if `TRUE`, add caption to the plot.
#' @param point_size size of point.
#' @param point_shape shape value of point.
#' @param color color for point and segment.
#' @param banded if `TRUE` (default), create banded background color.
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
#' p <- show_forest(lung,
#'   covariates = c("sex", "ph.ecog"), controls = "age", merge_models = TRUE,
#'   vars_to_show = "sex"
#' )
#' p
#' @testexamples
#' expect_s3_class(p, "ggplot")
show_forest <- function(data, covariates, controls = NULL,
                        time = "time",
                        status = "status",
                        merge_models = FALSE,
                        model_names = NULL,
                        vars_to_show = NULL,
                        drop_controls = FALSE,
                        add_caption = TRUE,
                        point_size = 3,
                        point_shape = 15,
                        color = "red",
                        banded = TRUE,
                        headings = list(variable = "Variable", n = "N", measure = "Hazard ratio", ci = NULL, p = "p"),
                        model_dir = file.path(tempdir(), "ezcox"),
                        verbose = TRUE,
                        ...) {
  fit_df <- ezcox(data,
    covariates = covariates,
    controls = controls,
    time = time, status = status,
    model_dir = model_dir,
    return_models = TRUE,
    verbose = verbose
  )
  fit_models <- get_models(fit_df)
  p <- show_models(fit_models,
    model_names = model_names,
    covariates = vars_to_show,
    merge_models = merge_models,
    drop_controls = drop_controls,
    headings = headings,
    format_options = forestmodel::forest_model_format_options(
      point_size = point_size, shape = point_shape, color = color, banded = banded
    ),
    ...
  )

  if (is.null(p)) {
    return(invisible(NULL))
  }

  if (add_caption) {
    if (is.null(controls)) {
      p <- p + ggplot2::labs(caption = paste("Cox analysis for variable", paste(covariates, collapse = " & ")))
    } else {
      p <- p + ggplot2::labs(caption = paste(
        "Cox analysis for variable", paste(covariates, collapse = " & "), "\n",
        "with", paste(controls, collapse = " & "), "controlled"
      ))
    }
  }

  p
}
