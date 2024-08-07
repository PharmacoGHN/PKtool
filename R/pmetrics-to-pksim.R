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
#' @param Molecule = molecule study, by default = NULL if none given


pm2psim <- function(
   x,
   sex = 0,
   time = "h",
   unit = "mg/L",
   tissue = "Plasma",
   compartment = "Peripheral Venous Blood",
   molecule = NULL,
   s_id = NULL,
   file = "Translate2pksim.csv"
) {

   # loading instruction for pmetrics datafile.
   df <- readr::read_csv(x, skip = 1)


   # re order data file to match pksim design
   ## rename column

   if (sex == 0) {
      # column type
      df_pksim <- cbind(df[3], df$OUT, df["new1"] <- NA, df["new2"] <- NA, df$EVID, df["Study ID"] <- NA, df["new3"] <- NA)

      # column names
      names(df_pksim)[1:7] <- c(paste0("Time [", time, "]"), paste0("Concentration [", unit, "]"),
                                "Tissue", "Compartment", "ID", "Study ID", "Molecule")

   } else {

      # column type
      df_pksim <- cbind(df$TIME, df$OUT, df["new1"] <- NA, df["new2"] <- NA, df$EVID, df["Study ID"] <- NA, df["new3"] <- NA, df[sex])

      # column names
      names(df_pksim)[c(1:8)] <- c(paste0("Time [", time, "]"), paste0("Concentration [", unit, "]"),
                                   "Tissue", "Compartment", "ID", "Study ID", "Molecule", "Sex")
   }

   # manage several output (such as Parent and metabolites)
   # if (metabolite = TRUE)
   df_pksim["Molecule"] <- molecule           # Fill molecule column

   # Row fill according to variable called in the argument
   df_pksim["Tissue"] <- tissue               # Fill tissue for all observation
   df_pksim["Compartment"] <- compartment     # Fill compartment for all observation
   df_pksim["Study ID"] <- s_id               # Fill study ID


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
