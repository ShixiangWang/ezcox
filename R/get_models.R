#' Get Model List from ezcox Object
#'
#' Models are renamed by the formulas.
#' @inheritParams filter_ezcox
#'
#' @return a named `list` with class `ezcox_models`
#' @export
#'
#' @examples
#' library(survival)
#' zz <- ezcox(lung, covariates = c("sex", "ph.ecog"), controls = "age", return_models = TRUE)
#' mds <- get_models(zz)
#' str(mds, max.level = 1)
get_models <- function(x) {
  stopifnot(inherits(x, "ezcox"))
  if (is.data.frame(x)) {
    stop("Please run ezcox() with return_models=TRUE firstly!")
  } else {
    models <- x$models$model
    model_names <- Map(function(x, y) {
      cc <- strsplit(y, ",")[[1]]
      paste("Surv ~", paste(c(x, cc), collapse = " + "))
    }, x = x$models$Variable, y = x$models$control)
  }
  names(models) <- model_names
  # Add variable attribute
  models <- Map(function(x, y) {
    attr(x, "Variable") <- y
    x
  }, x = models, y = x$models$Variable)
  class(models) <- c("ezcox_models", class(models))
  attr(models, "has_control") <- !all(is.na(x$models$control))
  models
}
