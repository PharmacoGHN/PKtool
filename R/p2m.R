#' pm2monolix
#'
#' 'pm2monolix' is a function intended to transform a pm file into a file readable by Monolix(R)
#'
#' @import usethis
#' @param input_file = dataset
#' @param iv = TRUE or FALSE
#' @param output_file = name given to the file. default = "translate.csv"
#' @param pm_vers a string argument that can take either "old" or "new". Refer to the datafile version of
#' Pmetrics. Old represent the version before 1.97.0
#'
#' @export


pm2monolix <- function(input_file = NULL,
                       iv = TRUE,
                       pm_vers = 'old',
                       output_file = "pm2monolix.csv"){

  # test if inputype is correct _______________________________________________________________________
  if(is.null(input_file) == T){
    rlang::abort(message = 'Error: no file provided')
  }

  if(is.character(input_file) == F){
    rlang::abort(message = "Error : input file name provided was not a character")
  }

  if(is.character(output_file) == F){
    rlang::abort(message = "Error : output file name provided was not a character")
  }

  if(is.character(pm_vers) == F){
    rlang::abort(message = "Error : the argument pm_vers provided was not a character")
  }
  # loading instruction for pmetrics datafile ________________________________________________________

  if(isTRUE(pm_vers == "old")){
    skip = 1
  } else { skip = 0}

  # check is file provided is csv
  if(isTRUE(tools::file_ext(input_file) == "csv")){
    df <- readr::read_csv(input_file, skip = skip,
                          show_col_types = FALSE)
  }

  # standardize all header into uppercase
  names(df) <- toupper(names(df))

  # re order data file to match monolix design

  # column type
  df_lixoft = cbind(df[1], df$EVID, df$DOSE, df$OUT, df$EVID, df$DUR, df$ADDL, df$II,
                    df[,(which(colnames(df)=="C3")+1):length(df[1,])])

  # rename column to match lixoft design
  names(df_lixoft)[1:8] = c('ID', 'TIME', 'AMOUNT', 'CONC', 'EVENT ID', 'TINF', 'ADDL', 'II')

  # if iv = F remove the duration col
  if(isFALSE(iv)){

    df_lixoft <- df_lixoft %>%
      dplyr::select(-TINF)
  }


  # save into csv
  utils::write.csv(df_lixoft, file = output_file, row.names = FALSE)
}
