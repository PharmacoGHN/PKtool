test_that("pm_autcomplete return the appropriate output", {
  test <- read.csv(test_path("testdata/PMtestv2_minimal.csv"))
  colnames(test) <- tolower(colnames(test))
  output <- pm_autocomplete(test, missing_var = c("evid", "addl", "ii", "input", "outeq", "c0", "c1", "c2", "c3"))
  output_expected <- read.csv(test_path("testdata/PMtest_old.csv"), skip = 1, header = TRUE)
  colnames(output_expected) <- gsub("X.", "", colnames(output_expected))

  expect_true(inherits(output, "data.frame"))
  expect_equal(colnames(output), c("ID", "EVID", "TIME", "DUR", "DOSE", "ADDL", "II", "INPUT", "OUT", "OUTEQ", "C0", "C1", "C2", "C3"))
  expect_equal(output[-9, ], output_expected[-9, -c(15, 16)]) # remove the 9th row as it is a second compartment observation, which is not sustained by the function
})
