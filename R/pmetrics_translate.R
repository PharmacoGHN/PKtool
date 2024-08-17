# Descritpion
#
# This set of function use pmetrics object and transform them into other pharmacometrics files ready to use
# currently supported Software are Pmetrics, Monolix (and all the Lixoft suite), NONMEM (Mrgsolve, mlxr2 and all derivate) and PKsim
# For more information See the Readme file.

#' pm2monolix
#'
#' 'pm2monolix' will transform a pmetrics file into a file readable by Monolix(R)
#'
#' @param input_file datafile
#' @param iv is drug given by iv route ? (TRUE or FALSE)
#' @param output_file name given to the file. default = "translate.csv"
#' @param pm_vers a numeric arugment that can be either 1 or 2. It refers to "old" or "new" Pmetrics version. Set to 1 by default (all Pmetrics version before 1.97)
#' @param ... additionnal argument that can be passed on read function (see utils::read.csv() and readxl::read_excel())
#'
#' @author Romain Garreau
#' @export


pm2monolix <- function(input_file = NULL,
                       iv = TRUE,
                       pm_vers = 1,
                       output_file = "pm2monolix.csv",
                       ...) {
  # test for condition descriibed above _______________________________________________________________________
  if (is.null(input_file)) stop("Error: no file provided")
  if (isFALSE(is.character(output_file))) stop("Error : output file name provided was not a character")
  if (not_in(pm_vers, c(1, 2))) stop("Error : pm_vers can only be 1 or 2. see description ?PKtool::pm2monolix")

  # if path to file this function use PKtool::read_pmetrics  ________________________________________________________
  if (!is.character(input_file) && attributes(input_file)$software == "pm.object") pm.data <- input_file
  if (is.character(input_file)) pm.data <- read_pmetrics(input_file, pm_vers = pm_vers, ...)

  # create new numerical ID specially for Pmetrics but add a column with matching previous ID.
  # for each unique ID (not case sensitive) create new ID (id, id.new )
  # column type
  pm.data <- case_colname(pm.data, "lower")

  lixoft.data <- pm.data |>
    dplyr::select(-c("c0", "c1", "c2", "c3")) |>
    dplyr::relocate("out",  .after = "dose") |>
    dplyr::relocate("evid", .after = "out")  |>
    dplyr::relocate("dur",  .after = "evid")

  col_name <- c(
    "ID" = "id",  "TIME" = "time",  "AMOUNT" = "dose",  "CONC" = "out",
    "EVENT_ID" = "evid",  "TINF" = "dur",  "ADDL" = "addl",  "II" = "ii",
    "ADMINISTRATION_ID" = "input",  "OBSERVATION_ID" = "outeq"
  )

  lixoft.data <- dplyr::rename(lixoft.data, dplyr::all_of(col_name)) |> case_colname("upper")

  # check if there is only one input and one observation (outeq) compartment
  if (length(unique(lixoft.data$ADMINISTRATION_ID)) < 3) lixoft.data <- dplyr::select(lixoft.data, -"ADMINISTRATION_ID")
  if (length(unique(lixoft.data$OBSERVATION_ID)) < 3) lixoft.data <- dplyr::select(lixoft.data, -"OBSERVATION_ID")

  return(lixoft.data)
}


# to add :
# pmetrics to PKsim
# pmetrics to mrgsolve
# pmetrics to BestDose

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
#'
#' @noRd


pm2psim <- function(
    x,
    sex = 0,
    time = "h",
    unit = "mg/L",
    tissue = "Plasma",
    compartment = "Peripheral Venous Blood",
    molecule = NULL,
    s_id = NULL,
    file = "Translate2pksim.csv") {
  # loading instruction for pmetrics datafile.
  df <- readr::read_csv(x, skip = 1)


  # re order data file to match pksim design
  ## rename column

  if (sex == 0) {
    # column type
    df_pksim <- cbind(df[3], df$OUT, df["new1"] <- NA, df["new2"] <- NA, df$EVID, df["Study ID"] <- NA, df["new3"] <- NA)

    # column names
    names(df_pksim)[1:7] <- c(
      paste0("Time [", time, "]"), paste0("Concentration [", unit, "]"),
      "Tissue", "Compartment", "ID", "Study ID", "Molecule"
    )
  } else {
    # column type
    df_pksim <- cbind(df$TIME, df$OUT, df["new1"] <- NA, df["new2"] <- NA, df$EVID, df["Study ID"] <- NA, df["new3"] <- NA, df[sex])

    # column names
    names(df_pksim)[c(1:8)] <- c(
      paste0("Time [", time, "]"), paste0("Concentration [", unit, "]"),
      "Tissue", "Compartment", "ID", "Study ID", "Molecule", "Sex"
    )
  }

  # manage several output (such as Parent and metabolites)
  # if (metabolite = TRUE)
  df_pksim["Molecule"] <- molecule # Fill molecule column

  # Row fill according to variable called in the argument
  df_pksim["Tissue"] <- tissue # Fill tissue for all observation
  df_pksim["Compartment"] <- compartment # Fill compartment for all observation
  df_pksim["Study ID"] <- s_id # Fill study ID


  # save object into csv file.
  write.csv2(df_pksim, file = file, row.names = FALSE, sep = ",", dec = ".")
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

