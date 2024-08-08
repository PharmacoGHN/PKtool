test_that("read_pmetrics throw warnings and error", {
  expect_error(read_pmetrics(data = "test.yml", pm_vers = 1), "your file must be a csv or an excel (xls or xlsx).", fixed = TRUE)
  expect_warning(
    read_pmetrics(data = test_path("testdata/PMtest_old.csv")),
    "Warning: the version of Pmetrics was not provided and was set to < 2.0.0 by default."
  )
})

test_that("read_pmetrics recognize first gen Pmetrics", {
  # output format
  old_version_csv <- test_path("testdata/PMtest_old.csv")
  old_version_csv2 <- test_path("testdata/PMtest_old_csv2.csv")
  old_version_xlsx <- test_path("testdata/PMtest_old.xlsx")
  old_version_xls <- test_path("testdata/PMtest_old.xls")
  expect_type(read_pmetrics(old_version_csv, pm_vers = 1), "list")
  expect_type(read_pmetrics(old_version_csv2, pm_vers = 1), "list")
  expect_type(read_pmetrics(old_version_xlsx, pm_vers = 1), "list")
  expect_type(read_pmetrics(old_version_xls, pm_vers = 1), "list")
})

test_that("read_pmetrics recognize second gen Pmetrics", {
  new_version_csv <- test_path("testdata/PMtestv2.csv")
  new_version_xlsx <- test_path("testdata/PMtest.xlsx")
  new_version_xls <- test_path("testdata/PMtest.xls")
  expect_type(read_pmetrics(new_version_csv, pm_vers = 2), "list")
  expect_type(read_pmetrics(new_version_csv, pm_vers = 2), "list")
  expect_type(read_pmetrics(new_version_csv, pm_vers = 2), "list")
})

test_that("read_pmetrics : mandatory variable not present", {
  #test <- read.csv(test_path("testdata/PM_error.csv"))

  expect_error(
    read_pmetrics(test_path("testdata/PM_error.csv"), pm_vers = 2),
    "Some mandatory variable are missing. Please check that 'ID', 'TIME', 'DUR', 'DOSE' and 'OUT' are present.",
    fixed = TRUE
  )
})
