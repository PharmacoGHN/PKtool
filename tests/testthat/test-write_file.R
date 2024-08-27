testthat::test_that("return error if argument not in the list", {
   test_data <- data.frame(id = c(1,2), id2 = c(1,2))
   attr(test_data, "software") <- "none"
   #testthat::expect_error(write_file(software = "Pmetrics", ext = "xml"), "This file extension is not supported. Please select file among csv, xlsx, xls or tsv")
   testthat::expect_error(write_file(), "Error: there is no object to save.")
   testthat::expect_error(write_file("test"), "Error: give a the path where to save the file.")
   testthat::expect_error(write_file(test_data, "filename.ext"), "This file extension is not supported. Please select file among csv, xlsx, xls or tsv", fixed = TRUE)
   testthat::expect_error(write_file(test_data, "filename.csv"), "Please select a software from Pmetrics, Lixoft, PKsim, or NONMEM.")
})
