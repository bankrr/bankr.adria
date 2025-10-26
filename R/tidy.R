#' Tidy bank statement
#' @param dat A data.frame as returned by [read()].
#' @export
tidy <- function(dat) {
  validate(dat)
  colnames(dat) <- tolower(colnames(dat))
  colnames(dat)[colnames(dat) == "x"] <- "currency"
  colnames(dat)[colnames(dat) == "data"] <- "date"
  colnames(dat) <- sub("\\.", "_", colnames(dat))

  dat_sub <- dat[, setdiff(colnames(dat), "x.1")]

  dat_sub[["date"]] <- as.Date(dat_sub[["date"]], format = "%d/%m/%Y")
  dat_sub[["valuta"]] <- as.Date(dat_sub[["valuta"]], format = "%d/%m/%Y")

  # Clean DARE column: remove dots, replace comma with dot
  if ("dare" %in% colnames(dat_sub)) {
    dat_sub[["dare"]] <- gsub("\\.", "", dat_sub[["dare"]])
    dat_sub[["dare"]] <- gsub(",", ".", dat_sub[["dare"]])
  }

  # Clean AVERE column: remove dots, replace comma with dot
  if ("avere" %in% colnames(dat_sub)) {
    dat_sub[["avere"]] <- gsub("\\.", "", dat_sub[["avere"]])
    dat_sub[["avere"]] <- gsub(",", ".", dat_sub[["avere"]])
  }

  # Convert DARE and AVERE to numeric
  # if ("dare" %in% colnames(dat_sub)) {
  #   dat_sub[["dare"]] <- as.numeric(dat_sub[["dare"]])
  #   if (anyNA(dat_sub[["dare"]])) {
  #     stop("Failed to convert DARE values to numeric. Check for invalid number formats.")
  #   }
  # }

  if ("avere" %in% colnames(dat_sub)) {
    dat_sub[["avere"]] <- as.numeric(dat_sub[["avere"]])
    if (anyNA(dat_sub[["avere"]])) {
      stop(
        "Failed to convert AVERE values to numeric. Check for invalid number formats."
      )
    }
  }

  # Remove balance rows that are not actual transactions using regex
  if ("descrizione_operazione" %in% colnames(dat_sub)) {
    balance_pattern <- "^(Saldo contabile|Saldo liquido|Disponibilit\u00e0 al|Saldo SBF per conti unici al|Saldo iniziale)"
    exclude_rows <- grepl(
      balance_pattern,
      dat_sub[["descrizione_operazione"]],
      perl = TRUE
    )
    dat_sub <- dat_sub[!exclude_rows, ]
  }

  if (is_pkg_avail("tibble")) {
    dat_sub <- tibble::as_tibble(dat_sub)
  }

  dat_sub
}

#' Tidy CBI
#' @param x A character vector as returned by [read_cbi()].
#' @return A data frame
#' @export
tidy_cbi <- function(x) {
  stopifnot(is.character(x))

  # Extract account info from summary record
  summary_record <- x[is_summary(x)]
  trans <- x[is_transaction(x) | is_continuation(x)]

  dat <- data.frame(
    id = xtr_record_number(trans),
    cin = xtr_cin(summary_record),
    bank_code = xtr_bank_code(summary_record),
    branch_code = xtr_branch_code(summary_record),
    account_number = xtr_account_number(summary_record),
    date_transaction = as_date(xtr_transaction_date(trans)),
    date_value = as_date(xtr_value_date(trans)),
    credit_debit = xtr_debit_credit(trans),
    description = trimws(xtr_description(trans), "both"),
    amount = as.numeric(gsub(",", ".", xtr_amount(trans))),
    stringsAsFactors = FALSE
  )

  dat$amount <- ifelse(
    dat$credit_debit == "D" & !is.na(dat$credit_debit),
    -dat$amount,
    dat$amount
  )

  out <- dat[, setdiff(names(dat), "credit_debit"), drop = FALSE]

  if (is_pkg_avail("tibble")) {
    out <- tibble::as_tibble(flatten(out))
  }

  out
}
