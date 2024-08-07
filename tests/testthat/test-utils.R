testthat::test_that("not_in operator works", {
   testthat::expect_equal(not_in("2", c("1", "22", "34")), TRUE)
   testthat::expect_equal(not_in("2", c("1", "2", "34")), FALSE)
})
