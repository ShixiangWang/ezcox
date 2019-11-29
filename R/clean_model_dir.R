#' Clean ezcox Model File Directory
#'
#' @inheritParams ezcox
#'
#' @return nothing
#' @export
#'
#' @examples
#' clean_model_dir
clean_model_dir <- function(model_dir = file.path(tempdir(), "ezcox")) {
  if (dir.exists(model_dir)) {
    message("Cleaning...")
    z <- unlink(model_dir, recursive = TRUE)
  } else {
    message("Path does not exist, skipping...")
  }
  message("Done")
}
