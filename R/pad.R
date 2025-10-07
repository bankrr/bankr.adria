pad_calc <- function(x, y, nchar_tot = 120L) {
  stopifnot(is_scalar_character(x), is_scalar_character(y))
  nchar_delta <- nchar_tot - (nchar(x) + nchar(y))
  if (nchar_delta < 0) {
    stop("String exceed maximum number of character")
  }
  nchar_delta
}

# TODO:
# Takes an arbitrary numer of character string
# If even number of string, pad in between putting the first and last item to the beginning and to the end
# If odd then start alternating string and pad from the left. Continue until nchar_tot is reached
pad_str <- function(x, nchar_tot = 120L) {
  stopifnot(
    is.character(x),
    "Length greater than two not supported yet" = length(x) == 2L
  )
  nchr <- nchar(paste0(x, collapse = ""))

  if (nchr > nchar_tot) {
    stop("String exceed maximum number of character")
  }

  if (!is_odd(length(x))) {
    n <- pad_calc(x[[1]], x[[2]], nchar_tot = nchar_tot)
    pad <- paste0(
      character(n),
      collapse = " "
    )
  }
  paste0(x[[1L]], pad, x[[2L]])
}
