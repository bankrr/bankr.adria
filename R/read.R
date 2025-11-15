#' Read bank statement
#' @param path A path.
#' @return A data frame
#' @export
read <- function(path) {
  if (!(is.character(path) && length(path) == 1)) {
    stop("Path is not a character of length one.")
  }
  if (!file.exists(path)) {
    stop("File does not exist.")
  }
  dat <- read.csv2(path)
  out <- validate(dat)
  out
}
