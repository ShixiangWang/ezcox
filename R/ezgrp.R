ezgrp <- function(data, grp_var, covariate, controls = NULL,
                  time = "time", status = "status", verbose = TRUE) {
  z <- data %>%
    dplyr::group_by(.data[[grp_var]]) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      mds = purrr::map2(.data$data, .data[[grp_var]],
                        function(data, var, covariate, controls = NULL,
                                 time = "time", status = status, verbose = FALSE) {
                          ## modify covariable name
                          colnames(data)[colnames(data) == covariate] <- var
                          ezcox(data = data, covariates = var,
                                controls = controls,
                                time = time, status = status,
                                return_models = TRUE,
                                verbose = verbose)
                        },
                        covariate = covariate,
                        controls = controls,
                        time = time,
                        status = status,
                        verbose = verbose)
    )

  md_list <- purrr::transpose(z$mds) %>% purrr::map(dplyr::bind_rows)
  ## show_models
}