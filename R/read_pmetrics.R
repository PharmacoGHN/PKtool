#' read_pmetrics
#'
#' @description
#' this function purpose is to read and extract data from a pmetrics file.
#' It is the first step before any translation of a Pmetrics type file into
#' another software type.
#'
#' @param data is the name of the file to read (csv, xlsx and xls file supported)
#' @param pm_vers correspond to the version of Pmetrics. Can take either 1 for all version before the 2.0.0 or 2 for all version above the 2.0.0
#' @param ... additionnal argument that can be passed on read function (see utils::read.csv() and readxl::read_excel())
#'
#' @author Romain Garreau
#' @export


read_pmetrics <- function(data, pm_vers = NULL, ...) {
  # set the default to legacy Pmetrics version
  if (is.null(pm_vers)) {
    rlang::warn(message = "Warning: the version of Pmetrics was not provided and was set to < 2.0.0 by default.")
    pm_vers <- 1
  }

  skip <- ifelse(pm_vers == 1, 1, 0) # load csv file and skip the first row if pmetrics ver < 2 (POPDATA DEC_11)
  pm_data <- auto_read(data, skip = skip, sep = ";")

  # if csv file with semi colon delimiters, read.csv will return a file with ncol == 1
  if (length(pm_data) == 1) {
    pm_data <- auto_read(data, skip = skip)
  }

  file_info <- pm_check_colname(pm_data)
  # add check to verify colname, if number, return pm_vers = 2 and reload file
  if (!file_info$check$mandatory) stop("Some mandatory variable are missing. Please check that 'ID', 'TIME', 'DUR', 'DOSE' and 'OUT' are present.")
  # add missing column that are not mandatory in Pmetrics V2

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
#' @author Romain Garreau
#' @export 

pm_check_colname <- function(data) {

  pm_colnames <- gsub("x.|#", "", tolower(colnames(data))) # remove the "x." created because of "#" in old Pmetrics files
  expected_pm_colnames <- c("id", "evid", "time", "dur", "dose", "addl", "ii", "input", "out", "outeq", "c0", "c1", "c2", "c3")

  # extract existing variable *var_name*
  var_pos <- which(pm_colnames %in% expected_pm_colnames)
  var_names <- pm_colnames[var_pos]

  # check if the 5 mandatory variable are present. NB : the check is not case sensitive
  mandatory_pm_var <- c("id", "time", "dur", "dose", "out")
  is_mandatory <- ifelse(all(mandatory_pm_var %in% var_names), TRUE, FALSE)

  # check if covariate are present and get their position
  cov_pos <- which(not_in(pm_colnames, expected_pm_colnames))
  cov_name <- pm_colnames[cov_pos]

  # check if the data is complete (all expected colnames are present)
  is_complete <- ifelse(all(expected_pm_colnames %in% pm_colnames), TRUE, FALSE)

  return(
    list(
      check = list(mandatory = is_mandatory, complete = is_complete),
      var = list(name = var_names, pos = var_pos),
      covar = list(name = cov_name, pos = cov_pos)
    )
  )

}



#' pm_autocomplete
#'
#' @description
#' this function will add the necessary covariate in order to translate an incomplete Pmetrics file (Version 2 only)
#'
#' @param data correspond to the data imported to run Pmetrics. Must be a dataframe
#'
#' @author Romain Garreau
#' @export


pm_autocomplete <- function(data) {
  # extract the column names
  var_names_raw <- colnames(data)
  expected_names <- c("id", "evid", "time", "dur", "dose", "addl", "ii", "input", "out", "outeq", "c0", "c1", "c2", "c3")

  # get the rank of C3 that is the last variable in the dataset
  # check the minimum variable requierment for 2.0.0 Pmetrics file and create test to exclude covariates

  # return the rank of absent covar
  rank_test <- grepl(paste(expected_names, collapse = "|"), tolower(var_names_raw))

  # To handle this, we need to check which names are not present in the dataset and to create a rule to add the
  # right variable. For some variable return a warning to tell that some mandatory variable are missing
  # Can't create, ID, EVID, Time, DOSE and OUT
}


# add read_monolix ; read_pksim; read_nonmem