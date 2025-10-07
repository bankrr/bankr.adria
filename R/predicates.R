is_scalar_character <- function(x) {
  if (is.character(x) && length(x) == 1L) TRUE else FALSE
}

is_odd <- function(x) {
  if (x %% 2L == 1L) TRUE else FALSE
}
