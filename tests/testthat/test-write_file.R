testthat::test_that("return error if argument not in the list", {
   testthat::expect_error(write_file(software = "Phoenix", ext = "csv"), "Please select a software from Pmetrics, Lixoft, PKsim, or NONMEM. The other software are not supported")
   testthat::expect_error(write_file(software = "Pmetrics", ext = "xml"), "This file extension is not supported. Please select file among csv, xlsx, xls or tsv")
   testthat::expect_error(write_file(), "Error : please check that only one object was passed to software and ext argument")
})
