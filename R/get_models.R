#' Get Model List from ezcox Object
#'
#' Models are renamed by the formulas.
#' @inheritParams filter_ezcox
#' @param variables a character vector representing variables to select.
#' @return a named `list` with class `ezcox_models`
#' @export
#'
#' @examples
#' library(survival)
#' zz <- ezcox(lung, covariates = c("sex", "ph.ecog"), controls = "age", return_models = TRUE)
#' mds <- get_models(zz)
#' str(mds, max.level = 1)
get_models <- function(x, variables = NULL) {
  stopifnot(inherits(x, "ezcox"))
  if (is.data.frame(x)) {
    stop("Please run ezcox() with return_models=TRUE or keep_models=TRUE firstly!")
  }
  model_df <- x$models
  if (!is.null(variables)) {
    model_df <- model_df %>%
      dplyr::filter(.data$Variable %in% variables)
  }
  if (ncol(model_df) < 4) {
    models <- purrr::map_df(model_df$model_file, function(x) {
      readRDS(x)
    })
    model_df <- dplyr::left_join(model_df, models, by = "Variable")
  }

  status_index <- which(!model_df$status)
  if (length(status_index) > 0) {
    message("Skipping the following failed variables:")
    message("\t", paste0(model_df$Variable[status_index], collapse = ", "))
    model_df <- dplyr::filter(model_df, model_df$status)
  }
  models <- model_df$model

  model_names <- Map(function(x, y) {
    cc <- strsplit(y, ",")[[1]]
    paste("Surv ~", paste(c(x, cc), collapse = " + "))
  }, x = model_df$Variable, y = model_df$control)

  names(models) <- model_names
  # Add variable attribute
  models <- Map(function(x, y) {
    attr(x, "Variable") <- y
    x
  }, x = models, y = model_df$Variable)
  class(models) <- c("ezcox_models", class(models))
  attr(models, "has_control") <- !all(is.na(model_df$control))
  models
}
