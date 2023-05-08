# https://stackoverflow.com/questions/8396577/check-if-character-value-is-a-valid-r-object-name
isValidAndUnreserved <- function(string) {
  sapply(string, function(x) {
    if (grepl("[*:|\\(\\)]", x)) {
      if (grepl("[+-\\*/:|\\)]{2,}", x)) {
        FALSE
      } else TRUE
    } else {
      make.names(x) == x
    }
  })
}

# Modify from https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
split_vector <- function(x, batch_size) {
  split(x, ceiling(seq_along(x) / batch_size))
}

utils::globalVariables(
  "estimate2"
)
.x = utf8::output_utf8