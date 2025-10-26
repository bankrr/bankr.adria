test_that("tidy_cbi works", {
  x <- readLines(pkg_file("testdata/cbi.txt"))
  dat <- tidy_cbi(x)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 3L)
  dat_expected <- data.frame(
    id = c("001", "002", "003"),
    cin = c("A", "A", "A"),
    bank_code = c("08982", "08982", "08982"),
    branch_code = c("62320", "62320", "62320"),
    account_number = c("034000000459", "034000000459", "034000000459"),
    date_transaction = as.Date(c("2025-06-04", "2025-06-04", "2025-06-11")),
    date_value = as.Date(c("2025-06-04", "2025-06-04", "2025-06-11")),
    description = c(
      "DISPOSIZIONE BONIFICO - SCT Coordinate benef: IT91W0398282920031000001999 A fav: I.E. Timpanni srl - Via 7 X.lli zervi, 99 - Abano Terme ID.MSG.:BQ53eKmU04062025094232 ID.END TO END:270525-113603-1-315-D001 RICON.1.: Fatt. FPR 221/25",
      "DISPOSIZIONE BONIFICO - SCT Coordinate benef: IT68J0898262310034000001509 A fav: Iconomadigimon srl - Via Xyz Conchig, 6/7 - Abano Terme ID.MSG.:ee1ZSwwP04062025095841 ID.END TO END:040625-094419-1-31-D001 RICON.1.: Fatt. FPR 682/25",
      "Ordinante: AIRPLANE OBLOT Causale:ACCREDITO BONIFICO - SCT Rata extra 2024/2025 a Saldo 5913865620100000480160062680IT Data ordine: 11/06/2025 IBAN ordinante: IT89S0307501603CC8009118233 Indirizzo ordinante: VIA XYZ DELAINE 1-A - MONTEGROTTO TERMEID End to End: NOTPROVIDED"
    ),
    amount = c(-456.1, -1294.88, 211.26)
  )
  expect_equal(as.data.frame(dat), dat_expected)
})
