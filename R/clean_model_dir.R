#' Clean ezcox Model File Directory
#'
#' @inheritParams ezcox
#'
#' @return nothing
#' @export
#'
#' @examples
#' \donttest{
#' clean_model_dir()
#' }
#' @testexamples
#' expect_message(clean_model_dir(), "Done")
clean_model_dir <- function(model_dir = file.path(tempdir(), "ezcox")) {
  if (dir.exists(model_dir)) {
    message("Cleaning...")
    z <- unlink(model_dir, recursive = TRUE)
  } else {
    message("Path does not exist, skipping...")
  }
  message("Done")
}
