# Error and warning Checks
test_that("read_pmetrics throw warnings and error", {
  expect_error(read_pmetrics(data = "test.yml", pm_vers = 1), "your file must be a csv or an excel (xls or xlsx).", fixed = TRUE)
  expect_warning(
    read_pmetrics(data = test_path("testdata/PMtest_old.csv")),
    "Warning: the version of Pmetrics was not provided and was set to < 2.0.0 by default."
  )
})

test_that("read_pmetrics : mandatory variable not present", {
  expect_error(
    read_pmetrics(test_path("testdata/PM_error.csv"), pm_vers = 2),
    "Some mandatory variable are missing. Please check that 'ID', 'TIME', 'DUR', 'DOSE' and 'OUT' are present.",
    fixed = TRUE
  )
})



# Pmetrics < 1.97.0 ----
test_that("read_pmetrics recognize first gen Pmetrics (csv)", {
  old_version_csv <- test_path("testdata/PMtest_old.csv")
  output_csv_old <- read.csv(old_version_csv, skip = 1, header = TRUE)
  expect_true(inherits(read_pmetrics(old_version_csv, pm_vers = 1), "data.frame"))
  expect_equal(read_pmetrics(old_version_csv, pm_vers = 1), output_csv_old)
})

test_that("read_pmetrics recognize first gen Pmetrics (csv EUR)", {
  old_version_csv2 <- test_path("testdata/PMtest_old_csv2.csv")
  #output_csv_old <- read.csv(old_version_csv, skip = 1, header = TRUE)
  expect_true(inherits(read_pmetrics(old_version_csv2, pm_vers = 1), "data.frame"))
  #expect_equal(read_pmetrics(old_version_csv, pm_vers = 1), output_csv_old)
})

test_that("read_pmetrics recognize first gen Pmetrics (XLSX)", {
  old_version_xlsx <- test_path("testdata/PMtest_old.xlsx")
  #output_csv_old <- read.csv(old_version_csv, skip = 1, header = TRUE)
  expect_true(inherits(read_pmetrics(old_version_xlsx, pm_vers = 1), "data.frame"))
  #expect_equal(read_pmetrics(old_version_csv, pm_vers = 1), output_csv_old)
})

test_that("read_pmetrics recognize first gen Pmetrics (xls)", {
  old_version_xls <- test_path("testdata/PMtest_old.xls")
  #output_csv_old <- read.csv(old_version_csv, skip = 1, header = TRUE)
  expect_true(inherits(read_pmetrics(old_version_xls, pm_vers = 1), "data.frame"))
  #expect_equal(read_pmetrics(old_version_csv, pm_vers = 1), output_csv_old)
})



# Pmetrics > 1.97.0 ----
test_that("read_pmetrics recognize second gen Pmetrics (csv)", {
  new_version_csv <- test_path("testdata/PMtestv2.csv")
  minimal_wo_cov <- test_path("testdata/PMtestv2_minimal.csv")
  minimal_with_cov <- test_path("testdata/PMtestv2_minimal_cov.csv")
  expected_output_new_version <- read.csv(new_version_csv) #full file nothing to change
  expected_output_wo_cov <- read.csv(minimal_wo_cov) # minimal data no covariate
  expected_output_with_cov <- read.csv(minimal_with_cov) # minimal data with covariate

  expect_true(inherits(read_pmetrics(new_version_csv, pm_vers = 2), "data.frame"))
  expect_equal(read_pmetrics(new_version_csv, pm_vers = 2), expected_output_new_version)
  expect_equal(read_pmetrics(minimal_wo_cov, pm_vers = 2)[-9, ], expected_output_new_version[-9, -c(15:16)])
  expect_equal(read_pmetrics(minimal_with_cov, pm_vers = 2)[-9, ], expected_output_new_version[-9, ])

})

test_that("read_pmetrics recognize second gen Pmetrics (XLSX)", {
  new_version_xlsx <- test_path("testdata/PMtest.xlsx")
  expect_true(inherits(read_pmetrics(new_version_xlsx, pm_vers = 2), "data.frame"))
  #expect_equal(read_pmetrics(minimal_with_cov, pm_vers = 2)[-9, ], expected_output_new_version[-9, ])
})

test_that("read_pmetrics recognize second gen Pmetrics (XLS)", {
  new_version_xls <- test_path("testdata/PMtest.xls")
  expect_true(inherits(read_pmetrics(new_version_xls, pm_vers = 2), "data.frame"))
  #expect_equal(read_pmetrics(minimal_with_cov, pm_vers = 2)[-9, ], expected_output_new_version[-9, ])
})
