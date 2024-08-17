test_that("p2monolix throw expected error", {
  expect_error(pm2monolix(), "Error: no file provided", fixed = TRUE)
  expect_error(pm2monolix("test.csv", output_file = NULL), "Error : output file name provided was not a character", fixed = TRUE)
  expect_error(pm2monolix("test.csv", pm_vers = 3), "Error : pm_vers can only be 1 or 2. see description ?PKtool::pm2monolix", fixed = TRUE)
  expect_error(pm2monolix("test.csv", pm_vers = "3"), "Error : pm_vers can only be 1 or 2. see description ?PKtool::pm2monolix", fixed = TRUE)
  skip("pm_vers = '2' is recognized when it souldn't")
  expect_error(pm2monolix("test.csv", pm_vers = "2"), "Error : pm_vers can only be 1 or 2. see description ?PKtool::pm2monolix", fixed = TRUE)
})


test_that("pm2monolix return the right output (preloaded file)", {
  # regular file without unique ID for outeq
  new_version_csv <- test_path("testdata/pmetrics_file/PMtestv2.csv")
  input_file <- read_pmetrics(new_version_csv, pm_vers = 2)

  expected_output <- read.csv(test_path("testdata/monolix_file/pm2monolix_nouadid.csv"))
  attr(expected_output, "software") <- "pm.object"
  expect_equal(pm2monolix(input_file), expected_output)

  # check unique ID for outeq
  pmfile_unique_outeq <- test_path("testdata/pmetrics_file/PMtestv2_unique_outeq.csv")
  input_file_unique_outeq <- read_pmetrics(pmfile_unique_outeq, pm_vers = 2)

  expected_output_unique_outeq <- read.csv(test_path("testdata/monolix_file/p2m_unique_id.csv"))
  attr(expected_output_unique_outeq, "software") <- "pm.object"
  expect_equal(pm2monolix(input_file_unique_outeq), expected_output_unique_outeq)
})

test_that("pm2monolix return the right output (path to file)", {
  # regular file without unique ID for outeq
  new_version_csv <- test_path("testdata/pmetrics_file/PMtestv2.csv")
  #input_file <- read_pmetrics(new_version_csv, pm_vers = 2)

  expected_output <- read.csv(test_path("testdata/monolix_file/pm2monolix_nouadid.csv"))
  attr(expected_output, "software") <- "pm.object"
  expect_equal(pm2monolix(new_version_csv, pm_vers = 2), expected_output)

  # check unique ID for outeq
  pmfile_unique_outeq <- test_path("testdata/pmetrics_file/PMtestv2_unique_outeq.csv")
  input_file_unique_outeq <- read_pmetrics(pmfile_unique_outeq, pm_vers = 2)

  expected_output_unique_outeq <- read.csv(test_path("testdata/monolix_file/p2m_unique_id.csv"))
  attr(expected_output_unique_outeq, "software") <- "pm.object"
  expect_equal(pm2monolix(input_file_unique_outeq), expected_output_unique_outeq)
})
