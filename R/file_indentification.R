# Description
#
# This function is here to identify file type in order
# to return the right write and reading function based on
# the type of file presented.
#
# ex : if a file was created with excel, the extension could either be
#' .xls or .xslx but would open with specific function.
#' The same applies to read.csv and read.csv2
#'


#' ext_bank
#'
#' @description
#' return the type of software and function to use
#' based on file extention
#'
#' @param file input file


ext_bank <- function(file) {

   sep_ext <- strsplit(x = file, split = "[.]")
   get_ext <- sep_ext[[1]][length(sep_ext[[1]])]

# loading instruction for pmetrics datafile.
# need to find a way around the skip (only in case of old Pmetrics data strucure)
   if (stringr::str_ends(file, ".csv")) {
      df <- read.csv(file, skip = 1)
      # if the dataframe length is one this means the sep != ","
      if (length(df) == 1) {
         df <- read.csv2(file, skip = 1)
      }
   }

   if (stringr::str_ends(file, ".xls")) {
      df <- readxl::read_excel(file, skip = 1)
   }

   if (stringr::str_ends(file, ".xlsx")) {
      df <- readxl::read_excel(file, skip = 1)
   }

}
