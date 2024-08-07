#' read_pmetrics
#'
#' @description
#' this function purpose is to read and extract data from a pmetrics file.
#' It is the first step before any translation of a Pmetrics type file into
#' another software type.
#'
#' @param dataset is the name of the .csv file to read
#' @param pm_vers correspond to the version of Pmetrics.
#' Can take either 1 for all version before the 2.0.0 or 2 for all version above the 2.0.0
#'


read_pmetrics <- function(dataset, pm_vers = NULL) {
  if (!stringr::str_ends(dataset, ".csv")) {
    rlang::abort(message = "Error: the file provided was not a csv file")
  }

  # set the default to legacy Pmetrics version
  if (is.null(pm_vers)) {
    rlang::warn(message = "Warning: the version of Pmetrics was not provided and was set to < 2.0.0 by default.")
    pm_vers <- 1
  }

  # load csv file and skip the first row (POPDATA DEC_11)
  if (pm_vers == 1) {
    # read the csv
    pm_data <- utils::read.csv2(dataset, skip = 1)

    # check if the delimiters were actually comma, if not use the european separator
    if (length(pm_data) == 1) {
      pm_data <- utils::read.csv(dataset, skip = 1)
    }
  }

  # Load csv file without skipping the first row
  if (pm_vers == 2) {
    # read the csv
    pm_data <- utils::read.csv2(dataset)

    # check if the delimiters were actually comma, if not use the european separator
    if (length(pm_data) == 1) {
      pm_data <- utils::read.csv(dataset)
    }

    # add missing column that are not mandatory in Pmetrics V2
  }

  # rename first column to ID
  colnames(pm_data)[1] <- "ID"

  # pm_check_colname


  # check col and recreate


  pm_final_data <- list(
    id = pm_data[1],
    event_id = pm_data["EVID"],
    time = pm_data["TIME"],
    dur = pm_data["DUR"],
    dose = pm_data["DOSE"],
    addl = pm_data["ADDL"],
    ii = pm_data["II"],
    input = pm_data["INPUT"],
    out = pm_data["OUT"],
    error_coeff = pm_data[c("C0", "C1", "C2", "C3")],
    covariate = pm_data[c(15, length(pm_data))] # get C3 length and then extract everything beyond C3
  )


  return(pm_final_data)
}




#' pm_check_colname
#'
#' @description
#' function that check if the minimum colnames required to used Pmetrics are here.
#'
#' @param data is the data in which the colum names must be checked
#'

pm_check_colname <- function(data) {
  # if there the number of colunm is < 15 get all the colnames, otherwise check the first 15. (Pmetrics format)
  pm_colnames <- ifelse(length(colnames(data)) > 15, colnames(data)[1:15], colnames(data))
  expected_pm_colnames <- c("id", "evid", "time", "dur", "dose", "addl", "ii", "input", "out", "outeq", "c0", "c1", "c2", "c3")

  if (tolower(pm_colnames) %in% expected_pm_colnames) {
    if (not_in(tolower(pm_colnames), expected_pm_colnames)) {
      return(FALSE) # fix, if all the names are in the vector of expected name it will return TRUE even if not all elements are present
    } else {
      return(TRUE)
    }
  }
}



#' pm_autocomplete
#'
#' @description
#' this function will add the necessary covariate in order to translate an incomplete Pmetrics file (Version 2 only)
#'
#' @param data correspond to the data imported to run Pmetrics. Must be a dataframe
#'
#'


pm_autocomplete <- function(data) {
  # extract the column names
  var_names_raw <- colnames(data)
  expected_names <- c("id", "evid", "time", "dur", "dose", "addl", "ii", "input", "out", "outeq", "c0", "c1", "c2", "c3")

  # get the rank of C3 that is the last variable in the dataset
  # check the minimum variable requierment for 2.0.0 Pmetrics file and create test to exclude covariates

  # return the rank of absent covar
  rank_test <- grepl(paste(expected_names, collapse = "|"), tolower(var_names_raw))

  # To handle this is, we need to check which names are not present in the dataset and to create a rule to add the
  # right variable. For some variable return an warning to tell that some mandatory variable are missing
  # Can't create, ID, EVID, Time, DOSE and OUT
}
