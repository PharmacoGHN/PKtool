test_that("not_in operator works", {
  expect_equal(not_in("2", c("1", "22", "34")), TRUE)
  expect_equal(not_in("2", c("1", "2", "34")), FALSE)
})

test_that("auto_read return intended error", {
  expect_error(auto_read("test.js"), "your file must be a csv or an excel (xls or xlsx).", fixed = TRUE)
  expect_error(auto_read("test.csv", sep = ":"), "In case of csv separator must be '/t', ';' or ','", fixed = TRUE)
})

test_that("auto_read read csv and takes other argument", {
  expected_csv <- read.csv(test_path("testdata/PMtest_old.csv"), skip = 1)
  expected_csv2 <- read.csv2(test_path("testdata/PMtest_old_csv2.csv"), sep = ";", skip = 1)
  expect_equal(auto_read(test_path("testdata/PMtest_old.csv"), skip = 1), expected_csv)
  expect_equal(auto_read(test_path("testdata/PMtest_old_csv2.csv"), sep = ";", skip = 1), expected_csv2)
})

test_that("auto_read read excel files", {
  expected_xlsx <- readxl::read_xlsx(test_path("testdata/PMtest_old.xlsx"), skip = 1)
  expected_xls <- readxl::read_xls(test_path("testdata/PMtest_old.xls"), skip = 1)
  expect_equal(auto_read(test_path("testdata/PMtest_old.xlsx"), skip = 1), expected_xlsx)
  expect_equal(auto_read(test_path("testdata/PMtest_old.xls"), skip = 1), expected_xls)
})