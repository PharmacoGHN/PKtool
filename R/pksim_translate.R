#' pm2psim
#'
#' @description
#' This function allow to create a file ready to be used by PKsim
#' from a Pmetrics file.
#'
#' @param  x = data set
#' @param time = time unit of the observation. by default time = "h"
#' @param unit = unit of measured concentration. by default "mg/L"
#' @param sex = integer in 0:length(df), call number of the column corresponding to individual sex
#' @param file = name given to the file. default = "translate.csv"
#' @param s_id = study id, by default = NULL
#' @param tissue correspond to the tissue from which the concentration was sampled, by default = 'Plasma'
#' @param compartment = compartment in which the sample have been taken. default value = "Peripheral Venous Blood"
#' @param molecule = molecule study, by default = NULL if none given


psim2pm <- function(
    x,
    sex = 0,
    time = "h",
    unit = "mg/L",
    tissue = "Plasma",
    compartment = "Peripheral Venous Blood",
    molecule = NULL,
    s_id = NULL,
    file = "Translate2pksim.csv") {

}


# # function test ----
#  ## loading a test dataset
#   df <- readr::read_csv("PMtest.csv", skip = 1)

#  ## running the function
#   pm2psim("PMtest.csv", time = 'min', sex = 15, unit = 'µg/mL')


# # bug check up : ----
# # function : 1 warning (sep not working in write.csv2)
# # argument :
# #   - sex : ok
# #   - time : ok
# #   - unit : ok
# #   - file : ok
# #   - tissue : ok
# #   - compartment : ok
# #   - s.id : ok
# #   - Molecule : ok
