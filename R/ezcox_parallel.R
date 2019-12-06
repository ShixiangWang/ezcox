#' Parallelly Run Cox Analysis in Batch Mode
#'
#' @param data a `data.frame` containing variables, time and os status.
#' @param covariates column names specifying variables.
#' @param controls column names specifying controls.
#' @param time column name specifying time, default is 'time'.
#' @param status column name specifying event status, default is 'status'.
#' @param batch_size processing size in a batch.
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
#'
#' library(survival)
#' ezcox_parallel(lung, covariates = c("sex", "ph.ecog"), controls = "age")
ezcox_parallel <- function(data, covariates, controls = NULL,
                           time = "time", status = "status",
                           batch_size = 100,
                           global_method = c("likelihood", "wald", "logrank"),
                           keep_models = FALSE,
                           return_models = FALSE,
                           model_dir = file.path(tempdir(), "ezcox"),
                           parallel = TRUE,
                           verbose = FALSE) {
  stopifnot(is.data.frame(data))
  all_cols <- unique(c(covariates, controls, time, status))
  data <- data[, all_cols]
  var_list <- split_vector(covariates, batch_size)

  if (parallel) {
    if (length(covariates) < 200) {
      if (verbose) message("Warning: variable < 200, parallel computation is not recommended!")
    }

    if (!requireNamespace("furrr")) {
      stop("Please install 'furrr' package firstly!")
    }

    oplan <- future::plan()
    future::plan("multiprocess")
    on.exit(future::plan(oplan), add = TRUE)
    res <- furrr::future_map(var_list,
      ezcox_caller,
      data = data,
      controls = controls,
      time = time,
      status = status,
      global_method = global_method,
      keep_models = keep_models,
      return_models = return_models,
      model_dir = model_dir,
      verbose = verbose,
      .progress = TRUE
    )
  } else {
    res <- purrr::map(var_list,
      ezcox_caller,
      data = data,
      controls = controls,
      time = time,
      status = status,
      global_method = global_method,
      keep_models = keep_models,
      return_models = return_models,
      model_dir = model_dir,
      verbose = verbose
    )
  }

  if (!is.data.frame(res[[1]]) & any(sapply(res, length) > 1)) {
    res2 <- list()
    res2$res <- purrr::reduce(lapply(res, function(x) x$res), .f = dplyr::bind_rows)
    res2$models <- purrr::reduce(lapply(res, function(x) x$models), .f = dplyr::bind_rows)
  } else {
    res2 <- dplyr::bind_rows(res)
  }

  class(res2) <- c("ezcox", class(res2))
  attr(res2, "controls") <- controls

  return(res2)
}


ezcox_caller <- function(covariates, data, controls = NULL,
                         time = "time", status = "status",
                         global_method = c("likelihood", "wald", "logrank"),
                         keep_models = FALSE,
                         return_models = FALSE,
                         model_dir = file.path(tempdir(), "ezcox"),
                         verbose = TRUE) {
  ezcox(
    data = data,
    covariates = covariates,
    controls = controls,
    time = time,
    status = status,
    global_method = global_method,
    keep_models = keep_models,
    return_models = return_models,
    model_dir = model_dir,
    verbose = verbose
  )
}
