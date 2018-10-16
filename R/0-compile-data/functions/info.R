info <- new.env()
get_all_info <- function() as.list(info)
set_info <- function(key, value, label) {
  x <- value
  attr(x, "label") <- label
  info[[key]] <<- x
}
get_info <- function(key) {
  info[[key]]
}
