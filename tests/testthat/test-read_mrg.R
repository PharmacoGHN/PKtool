test_that("read_mrg throw the appropriate error", {
  testPath <- test_path("testdata/mrgsolve/mrgTestExtran.csv")
  testFile <- read.csv(testPath)
  #expected_output <- read_mrg(testFile)

  expect_error(read_mrg(1), "Error: data provided must be a dataframe or the path to the data", fixed = TRUE)
  expect_error(read_mrg(testFile[,-8]), "Some mandatory variable are missing. Please check that 'ID', 'TIME', 'AMT', 'CONC' and 'EVID' are present.", fixed = TRUE)

  # modify data to throw evid out of bound error
  evidError <- testFile
  evidError[7, 8] <- 7
  expect_error(read_mrg(evidError), "Error: some evid are outside of supported values. See mrgsolve data structure", fixed = TRUE)
})

test_that("read_mrg throw the appropriate error", {
  testPath <- test_path("testdata/mrgsolve/mrgTestExtran.csv")
  testFile <- read.csv(testPath)
  #expected_output <- read_mrg(testFile)
  attr(testFile, "software") <- "mrgsolve"

  expect_equal(read_mrg(testFile), testFile)
  expect_equal(read_mrg(testPath), testFile)
})