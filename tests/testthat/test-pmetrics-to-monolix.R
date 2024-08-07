
testthat::test_that("pm2monolix throw the adequate error and warnings", {

   testthat::expect_error(pm2monolix(file = "test_data.tsv"), "Error : the input file should be a .csv file")
})




testthat::test_that("return the adequate output with covariates", {
   skip()
   pm_old_version_input_no_cov <- data.frame(
      ID = c("1", "1", "1"),
      EVID = c("1", "1", "1"),
      TIME = c("0", "12", "24"),
      DUR = c("2", "2", "2"),
      DOSE = c("250", "250", "200"),
      ADDL = c(".", ".", "."),
      II = c(".", ".", "."),
      INPUT = c("1", "1", "1"),
      OUT = c(".", ".", "10.2"),
      OUTEQ = c(".", ".", "."),
      C0 = c(".", ".", "."),
      C1 = c(".", ".", "."),
      C2 = c(".", ".", "."),
      C3 = c(".", ".", ".")
   )

   expected_ouput_iv <- data.frame(
      ID = c("1", "1", "1"),
      TIME = c("0", "12", "24"),
      AMOUNT = c("250", "250", "200"),
      CONC = c(".", ".", "10.2"),
      EVEN_ID = c("1", "1", "1"),
      TINF = c("2", "2", "2"),
      ADDL = c(".", ".", "."),
      II = c(".", ".", ".")
   )

   testthat::expect_error(pm2monolix(file = "test_data.tsv"))
})
