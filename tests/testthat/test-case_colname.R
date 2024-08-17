test_that("case_colname throw adequate error and return the right output", {
  df_test_upper <- data.frame(ID1 = c("test", "test"), ID2 = c("test", "test"))
  df_test_lower <- data.frame(id1 = c("test", "test"), id2 = c("test", "test"))
  expect_error(case_colname("data.csv"), "Error: data provided is not a dataframe", fixed = TRUE)
  expect_error(case_colname(df_test_lower, "Camel"), "Error: Only 'lower' and 'upper' are supported", fixed = TRUE)
  expect_equal(case_colname(df_test_lower, "upper"), df_test_upper)
  expect_equal(case_colname(df_test_upper, "lower"), df_test_lower)
})
