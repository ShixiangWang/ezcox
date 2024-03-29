#' Show Cox Models
#'
#' @param models a `ezcox_models` from [get_models()] or a (named) list of Cox models.
#' @param model_names model names to show when `merge_models=TRUE`.
#' @param covariates a character vector optionally listing the variables to include in the plot (defaults to all variables).
#' @param merge_models if 'TRUE', merge all models and keep the plot tight.
#' @param drop_controls works when `covariates=NULL` and `models` is a `ezcox_models`, if `TRUE`,
#' it removes control variables automatically.
#' @param headings a `list` for setting the heading text.
#' @param ... other arguments passing to [forestmodel::forest_model()].
#'
#' @return a `ggplot` object
#' @importFrom utils packageVersion
#' @export
#'
#' @examples
#' library(survival)
#' zz <- ezcox(lung, covariates = c("sex", "ph.ecog"), controls = "age", return_models = TRUE)
#' mds <- get_models(zz)
#' show_models(mds)
#' show_models(mds, model_names = paste0("Model ", 1:2))
#' show_models(mds, covariates = c("sex", "ph.ecog"))
#' show_models(mds, drop_controls = TRUE)
#' show_models(mds, merge_models = TRUE)
#' p <- show_models(mds, merge_models = TRUE, drop_controls = TRUE)
#' p
#' @testexamples
#' expect_s3_class(p, "ggplot")
show_models <- function(models, model_names = NULL, covariates = NULL,
                        merge_models = FALSE, drop_controls = FALSE,
                        headings = list(variable = "Variable", n = "N", measure = "Hazard ratio", ci = NULL, p = "p"),
                        ...) {
  stopifnot(inherits(models, "ezcox_models") | all(sapply(models, function(x) inherits(x, "coxph"))), is.list(headings))

  if (is.null(headings$variable)) {
    headings$variable <- "Variable"
  }
  if (is.null(headings$n)) {
    headings$n <- "N"
  }
  if (is.null(headings$measure)) {
    headings$measure <- "Hazard ratio"
  }
  if (is.null(headings$p)) {
    headings$p <- "p"
  }

  pkg_version <- packageVersion("forestmodel")
  if (pkg_version$major == 0 & pkg_version$minor < 6) {
    message("Please install the recent version of forestmodel firstly.")
    message("Run the following command:")
    message("  remotes::install_github(\"ShixiangWang/forestmodel\")")
    message("Or")
    message("  remotes::install_git(\"https://gitee.com/ShixiangWang/forestmodel\")")
    return(invisible(NULL))
  }

  if (!is.null(model_names)) {
    names(models) <- model_names
  } else if (is.null(names(models))) {
    names(models) <- paste0("Model ", seq_along(models))
  }

  if (drop_controls) {
    if (is.null(covariates)) {
      message("covariates=NULL but drop_controls=TRUE, detecting controls...")
      if (isTRUE(attr(models, "has_control"))) {
        message("Yes. Setting variables to keep...")
        covariates <- sapply(models, function(x) attr(x, "Variable"))
      } else {
        message("No. Skipping...")
      }
    }
    message("Done.")
  }

  if (!is.null(covariates)) {
    covariates <- ifelse(isValidAndUnreserved(covariates) | startsWith(covariates, "`"),
      covariates, paste0("`", covariates, "`")
    )
  }

  forestmodel::forest_model(
    model_list = models,
    panels = cox_panel(headings = headings),
    covariates = covariates,
    merge_models = merge_models, ...
  )
}
