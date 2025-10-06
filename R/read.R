#' Read bank statement
#' @param path A path.
#' @export
read <- function(path) {
  if (!(is.character(path) && length(path) == 1)) {
    stop("Path is not a character of length one.")
  }
  if (!file.exists(path)) {
    stop("File does not exist.")
  }
  read.csv2(path)
}

#' Read CBI
#' @param path A path.
#' @export
read_cbi <- function(path) {
  if (!(is.character(path) && length(path) == 1)) {
    stop("Path is not a character of length one.")
  }
  out <- readLines(path)

  nc <- vapply(out, nchar, FUN.VALUE = integer(1), USE.NAMES = FALSE)

  if (any(nc != 120L)) {
    stop("Some lines does not contain 120 characters")
  }

  out
}
