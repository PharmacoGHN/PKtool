# Description
#
# This is a set of function designed to read NONMEM/mrgsolve type file and perform certain check before
# Changing the data into another format.
#
#
# In addition the function will check EVID values if present as only the following values are accepted.
# 0 = observation record
# 1 = dosing event (bolus or infusion)
# 2 = other type event, with solver stop and restart
# 3 = system reset
# 4 = reset and dose
# 8 = replace the amount in the compartment with amt

#' read_mrg
#'
#' read mrgsolve file types
#'
#' @param data mrgsolve format data type expected. Can be a path toward a file (csv, xls or xlsx) or a dataframe.
#' @param ... additionnal argument possible see ?PKtool::auto_read
#'
#' @author Romain Garreau
#' @export

read_mrg <- function(data, ...) {
  # if (!is.character(data) || !is.data.frame(data)) stop("Error: data provided must be a dataframe or the path to the data")
  # if (is.character(data)) mrg_data <- auto_read(data, ...)

  if (!is.data.frame(data)) {
    if (is.character(data)) mrg_data <- auto_read(data, ...)
    if (!is.character(data)) stop("Error: data provided must be a dataframe or the path to the data")
  } 

  if (is.data.frame(data)) mrg_data <- data
  
  file_info <- mrg_check_colname(mrg_data)
  if (!file_info$check$mandatory) stop("Some mandatory variable are missing. Please check that 'ID', 'TIME', 'AMT', 'CONC' and 'EVID' are present.")
  if (isFALSE(file_info$check$evid)) stop("Error: some evid are outside of supported values. See mrgsolve data structure")

  attr(mrg_data, "software") <- "mrgsolve"
  return(mrg_data)
}


#' mrg_check_colname
#'
#' check that colname are all adequate to use with mrg solve
#'
#' @param data data
#' @param idat Logical to specify if the dataset contains indivudal data or observation. Is False by default.
#'
#' @author Romain Garreau
#' @export


mrg_check_colname <- function(data, idat = FALSE) {

  data <- case_colname(data, "upper")
  expected_mrg_colnames <- c("ID", "TIME", "EVID", "AMT", "CMT", "RATE", "II", "ADDL", "SS", "CONC")
  mrg_colnames <- colnames(data)


  # extract existing variable *var_name*
  var_pos <- which(mrg_colnames %in% expected_mrg_colnames)
  var_names <- mrg_colnames[var_pos]
  var_missing <- expected_mrg_colnames[which(not_in(expected_mrg_colnames, mrg_colnames))]

  # check if the 5 mandatory variable are present. NB : the check is not case sensitive
  mandatory_pm_var <- c("ID", "TIME", "AMT", "EVID")
  if (idat) mandatory_pm_var <- c(mandatory_pm_var, "CONC")
  is_mandatory <- ifelse(all(mandatory_pm_var %in% var_names), TRUE, FALSE)

  # check if evid contain the right value
  if ("EVID" %in% mrg_colnames) evid_check <- ifelse(all(data$EVID %in% c("0", "1", "2", "3", "4", "8")), TRUE, FALSE)
  if (not_in("EVID", mrg_colnames)) evid_check <- FALSE

  # check if covariate are present and get their position
  cov_pos <- which(not_in(mrg_colnames, expected_mrg_colnames))
  cov_name <- mrg_colnames[cov_pos]

  # check if the data is complete (all expected colnames are present)
  is_complete <- ifelse(all(expected_mrg_colnames %in% mrg_colnames), TRUE, FALSE)

  return(
    list(
      check = list(mandatory = is_mandatory, complete = is_complete, evid = evid_check),
      var = list(name = var_names, pos = var_pos, missing = var_missing),
      covar = list(name = cov_name, pos = cov_pos)
    )
  )
}
