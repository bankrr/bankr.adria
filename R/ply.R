#' Type-stable apply
#'
#' Wrappers for [base::vapply()] for common types.
#'
#' @param x,fun,...,use_names Forwarded to [base::vapply()]
#' @param length Result length
#' @rdname ply
#'
#' @export
chr_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, character(length), ..., USE.NAMES = use_names)
}

#' @rdname ply
#' @export
lgl_ply <- function(x, fun, ..., length = 1L, use_names = FALSE) {
  vapply(x, fun, logical(length), ..., USE.NAMES = use_names)
}
