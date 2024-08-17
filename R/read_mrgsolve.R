# Description
#
# This is a set of function designed to read NONMEM/mrgsolve type file and perform certain check before
# Changing the data into another format.


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
  expecte_mrg_colnames <- c("ID", "TIME", "EVID", "AMT", "CMT", "RATE", "II", "ADDL", "SS")

  # check EVID if present. Only these values are accepted.
  # 0 = observation record
  # 1 = dosing event (bolus or infusion)
  # 2 = other type event, with solver stop and restart
  # 3 = system reset
  # 4 = reset and dose
  # 8 = replace the amount in the compartment with amt
}
