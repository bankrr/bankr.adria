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
    dat[,
      setdiff(
        names(dat),
        c("id", "cin", "bank_code", "branch_code", "account_number")
      ),
      drop = FALSE
    ],
    by = list(
      id = dat$id,
      cin = dat$cin,
      bank_code = dat$bank_code,
      branch_code = dat$branch_code,
      account_number = dat$account_number
    ),
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
