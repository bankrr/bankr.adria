as_date <- function(x) {
  stopifnot(
    is.character(x)
    # all(nchar(x) == 6)
  )
  as.Date(x, format = "%d%m%y")
}

xtr_match <- function(text, pattern) {
  m <- regexec(pattern, text, perl = TRUE)
  regmatches(text, m)
}

flatten <- function(dat) {
  stopifnot(is.data.frame(dat))
  aggregate(
    dat[, setdiff(names(dat), "id"), drop = FALSE],
    by = list(id = dat$id),
    FUN = function(x) {
      # TODO: too general
      if (is.character(x)) {
        paste0(x, collapse = "")
      } else {
        x[!is.na(x)][[1L]]
      }
    }
  )
}
