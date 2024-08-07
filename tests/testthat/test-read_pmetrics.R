test_that("read_pmetrics throw warnings and error", {
  expect_error(read_pmetrics(dataset = "test.xlsx", pm_vers = 1), "Error: the file provided was not a csv file")
  expect_warning(
    read_pmetrics(dataset = test_path("testdata/PMtest_old.csv")),
    "Warning: the version of Pmetrics was not provided and was set to < 2.0.0 by default."
  )
})

test_that("read_pmetrics differenciate between first and second gen Pmetrics", {
  # output format
  test_data <- test_path("testdata/PMtest_old.csv")
  expect_type(read_pmetrics(test_data, pm_vers = 1), "list")
  expect_equal(length(read_pmetrics(test_data, pm_vers = 1)), 11)
})

test_that("pm_check_colname return the rigth output", {
  test_true <- read.csv(test_path("testdata/PMtest_old.csv"), skip = 1)
  names(test_true)[1] <- "ID"
  test_false_arg_number <- test_true[, -c(10, length(test_true))] # only 10 of the 15 variables necessary
  test_false_arg_name <- test_true[, -c(6, 7)] # WT and AGE remain

  expect_true(pm_check_colname(test_true))

  skip(message = "Skipping due to bug (issue #8)")
  # test false if not all name are present
  expect_false(pm_check_colname(test_false_arg_number))

  # test false if one of the name is wrong
  expect_false(pm_check_colname(test_false_arg_name))
})
