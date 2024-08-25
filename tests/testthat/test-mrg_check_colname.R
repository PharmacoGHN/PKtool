test_that("multiplication works", {
  testFile <- read.csv(test_path("testdata/mrgsolve/mrgTestExtran.csv"))
  output_object <- mrg_check_colname(testFile)

  expect_type(output_object, "list")
  expect_true(output_object$check$evid)
  expect_true(output_object$check$mandatory)
  expect_false(output_object$check$complete)
  expect_equal(output_object$var$pos, c(1, 2, 3, 4, 5, 6, 7, 8))
  expect_equal(output_object$var$name, c("ID", "AMT", "CMT", "TIME", "ADDL", "II", "RATE", "EVID"))

  # check error on evid
  error_evid <- mrg_check_colname(testFile[-8])
  expect_false(error_evid$check$evid)
  
  # check if the data containt individual observation
  idat_error <- mrg_check_colname(testFile, idat = TRUE)
  expect_false(idat_error$check$mandatory)
})

# todo add test for :
#  covariate identification
#  covariate id in case on null output
#  test for individual covariate and simulation dataset