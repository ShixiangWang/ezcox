# https://stackoverflow.com/questions/8396577/check-if-character-value-is-a-valid-r-object-name
isValidAndUnreserved <- function(string) {
  make.names(string) == string
}

# Modify from https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
split_vector <- function(x, batch_size) {
  split(x, ceiling(seq_along(x) / batch_size))
}