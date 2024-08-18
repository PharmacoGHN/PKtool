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


#' mrg_check_colname
#'
#' check that colname are all adequate to use with mrg solve
#'
#' @param data data
#'
#' @author Romain Garreau
#' @noRd

mrg_check_colname <- function(data) {

  data <- case_colname(data, "upper")
  expecte_mrg_colnames <- c("ID", "TIME", "EVID", "AMT", "CMT", "RATE", "II", "ADDL", "SS", "CONC")
  mrg_colnames <- colnames(data)


  # extract existing variable *var_name*
  var_pos <- which(mrg_colnames %in% expected_mrg_colnames)
  var_names <- mrg_colnames[var_pos]
  var_missing <- expected_mrg_colnames[which(not_in(expected_mrg_colnames, mrg_colnames))]

  # check if the 5 mandatory variable are present. NB : the check is not case sensitive
  mandatory_pm_var <- c("ID", "TIME", "AMT", "CONC", "EVID")
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
