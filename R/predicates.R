is_scalar_character <- function(x) {
  if (is.character(x) && length(x) == 1L) TRUE else FALSE
}
