#' Group Cox Analysis and Visualization
#'
#' @inheritParams show_forest
#' @param grp_var a group column.
#' @param covariate a covariable for cox analysis.
#' @param add_all if `TRUE`, add a group for all data rows.
#' @param sort if `TRUE`, sort the models by the HR values.
#' @param decreasing logical, should the sort order be increasing or decreasing?
#'
#' @return a `list`.
#' @export
#' @examples
#' library(survival)
#' ezcox_group(lung, grp_var = "sex", covariate = "ph.ecog")
#' ezcox_group(lung, grp_var = "sex", covariate = "ph.ecog", controls = "age")
#' p <- ezcox_group(lung,
#'   grp_var = "sex", covariate = "ph.ecog",
#'   controls = "age", add_all = TRUE
#' )
#' @testexamples
#' expect_is(p, "list")
ezcox_group <- function(data, grp_var, covariate, controls = NULL,
                        time = "time", status = "status",
                        sort = FALSE,
                        decreasing = TRUE,
                        add_all = FALSE,
                        add_caption = TRUE,
                        verbose = TRUE,
                        headings = list(variable = "Group", n = "N", measure = "Hazard ratio", ci = NULL, p = "p"),
                        ...) {
  stopifnot(is.list(headings), length(grp_var) == 1L, length(covariate) == 1L)

  if (length(data[[grp_var]]) == length(table(data[grp_var]))) {
    stop("Cannot set 'grp_var' to a variable cannot be groupped!")
  }

  if (is.null(headings$variable)) {
    headings$variable <- "Group"
  }

  if (add_all) {
    data[[grp_var]] <- as.character(data[[grp_var]])
    data_all <- data
    data_all[[grp_var]] <- "ALL"
    data <- dplyr::bind_rows(data, data_all)
  }

  run_model <- function(data, grp_var, covariate, controls = NULL,
                        time = "time", status = status, verbose = FALSE) {
    var <- unique(as.character(data[[grp_var]]))
    data <- data[, c(covariate, controls, time, status)]
    ## modify covariable name
    colnames(data)[colnames(data) == covariate] <- var
    ezcox(
      data = data, covariates = var,
      controls = controls,
      time = time, status = status,
      return_models = TRUE,
      verbose = verbose
    )
  }

  md_list <- data %>%
    dplyr::group_split(.data[[grp_var]]) %>%
    purrr::map(run_model,
      grp_var = grp_var,
      covariate = covariate,
      controls = controls,
      time = time,
      status = status,
      verbose = verbose
    ) %>%
    purrr::transpose() %>%
    purrr::map(dplyr::bind_rows) %>%
    purrr::map(function(x) {
      x %>%
        dplyr::rename(Group = .data$Variable) %>%
        dplyr::mutate(Variable = covariate) %>%
        dplyr::select(c("Group", "Variable"), dplyr::everything())
    })

  names(md_list) <- c("stats", "models")
  class(md_list) <- "ezcox"

  if (sort) {
    md_orders <- md_list$stats %>%
      dplyr::filter(!.data$is_control) %>%
      dplyr::distinct(.data$Group, .keep_all = TRUE) %>%
      dplyr::filter(.data$Group != "ALL") %>%
      dplyr::pull(.data$HR)
    md_orders <- order(md_orders, decreasing = decreasing)

    md_list$models <- md_list$models %>%
      dplyr::slice(c(md_orders, setdiff(seq_len(nrow(md_list$models)), md_orders)))
  }

  fit_models <- get_models(md_list)

  ## show_models
  p <- show_models(fit_models, merge_models = TRUE, drop_controls = TRUE, headings = headings, ...)

  if (is.null(p)) {
    return(invisible(NULL))
  }

  if (add_caption) {
    if (is.null(controls)) {
      p <- p + ggplot2::labs(caption = paste("Univariable analysis for variable", covariate))
    } else {
      p <- p + ggplot2::labs(caption = paste(
        "Multivariable analysis for variable", covariate, "\n",
        "with", paste(controls, collapse = " & "), "controlled"
      ))
    }
  }

  message("Returns a list containing data and ggplot.")

  return(list(
    data = md_list,
    plot = p
  ))
}
