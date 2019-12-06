#' Filter ezcox
#'
#' @param x a `ezcox` object from [ezcox()].
#' @param levels levels to filter, default is 'auto', it will filter all control variables.
#' @param type default is 'both' for filtering both contrast level and reference level.
#' It can also be 'contrast' for filtering only contrast level and 'ref' for
#' filtering only reference level.
#' @author Shixiang Wang <w_shixiang@163.com>
#' @return a `ezcox` object
#' @export
#'
#' @examples
#' library(survival)
#' lung$ph.ecog <- factor(lung$ph.ecog)
#' zz <- ezcox(lung, covariates = c("sex", "age"), controls = "ph.ecog")
#' zz
#' filter_ezcox(zz)
#' filter_ezcox(zz, c("0", "2"))
#' filter_ezcox(zz, c("0", "2"), type = "contrast")
#' filter_ezcox(zz, c("0", "2"), type = "ref")
filter_ezcox <- function(x, levels = "auto", type = c("both", "contrast", "ref")) {
  stopifnot(inherits(x, "ezcox"))
  controls <- attr(x, "controls")
  type <- match.arg(type)

  if (is.null(controls)) {
    message("Nothing to be simpler!")
  } else {
    if (!is.data.frame(x)) {
      data <- x$res
    } else {
      data <- x
    }
    if (identical(levels, "auto")) {
      data <- dplyr::filter(data, !data$is_control)
    } else {
      message("Filtering control levels", " in '", type, "' mode:")
      message("\t", paste0(levels, collapse = ", "))

      if (type == "both") {
        index <- !(data$contrast_level %in% levels | data$ref_level %in% levels)
      } else if (type == "contrast") {
        index <- !data$contrast_level %in% levels
      } else {
        index <- !data$ref_level %in% levels
      }
      data <- data[index, ]
    }

    if (!is.data.frame(x)) {
      x$res <- data
      return(x)
    } else {
      return(data)
    }
  }
}
