#' @title pm2monolix
#'
#' @description
#' pm2monolix is a function that translate xlsx or csv file used by pmetrics
#' to lixoft format (Monolix and other software)
#' Deprecated.
#'
#' @param file correspond to the input file
#' @param admin_route correspond to the administration route
#' @param filename correspond to the output name wanted for the file
#' @param pm_vers correspond to the version of pmetrics by defaut = 2 (new version),
#'  if version < 2.0.0 argument shoud be 1
#'
#' @export

pm2monolix <- function(
    file,
    admin_route = TRUE,
    filename = "pm2monolix.csv",
    pm_vers = 2) {
  if (!stringr::str_ends(file, ".csv")) {
    rlang::abort(message = "Error : the input file should be a .csv file")
  }

  # loading instruction for pmetrics datafile.
  if (stringr::str_ends(file, ".csv")) {
    df <- readr::read_csv(file, skip = 1)
  }



  # re order data file to match monolix design
  ## modulate dataset according to the administration route
  ##  if no iv data, remove duration
  # (function call iv = FALSE, by default iv = TRUE)
  ## rename column

  if (admin_route == TRUE) {
    # column type
    df_translated <-
      cbind(df[1], df$TIME, df$DOSE, df$OUT, df$EVID, df$DUR, df$ADDL, df$II)
    # ,
    #          df[, 15:length(df[1, ])]) # covariates column
    # column names
    names(df_translated)[1:8] <-
      c("ID", "TIME", "AMOUNT", "CONC", "EVENT ID", "TINF", "ADDL", "II")
  } else {
    # column types
    df_translated <-
      cbind(
        df[1], df$TIME, df$DOSE, df$OUT, df$EVID, df$ADDL, df$II,
        df[, 15:length(df[1, ])]
      ) # covariates column
    # column names
    names(df_translated)[1:7] <-
      c("ID", "TIME", "AMOUNT", "CONC", "EVENT ID", "ADDL", "II")
  }

  return(df_translated)

  # manage several output (such as Parent and metabolites)
  # if (metabolite = TRUE)
}
