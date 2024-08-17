## NAMESPACE dependecies declaration -------------
#' @importFrom readr read_csv
#' @importFrom openxlsx write.xlsx read.xlsx
#' @importFrom utils write.csv2

NULL

#' not_in
#' @description not_in = not in operator
#'
#' @param a value to check can be list or string
#' @param b value in which a is searched for, can be a single string or list of strings
#'
#' @author Romain Garreau
#' @export

not_in <- function(a, b) {
  !a %in% b
}


#' auto_read
#'
#' @description
#' get the file extension and the separator values and call the adequate
#' function to read file.
#'
#' ex : if a file was created with excel, the extension could either be
#' .xls or .xslx but would open with specific function.
#' The same applies to read.csv and read.csv2
#'
#' @param file path to the file you want to import
#' @param sep which separator
#' @param ... additionnal argument that can be passed on read function (see utils::read.csv() and readxl::read_excel())
#'
#' @author Romain Garreau
#' @export

auto_read <- function(file, sep = ",", ...) {
  file_extension <- tools::file_ext(file)

  if (not_in(file_extension, c("csv", "xls", "xlsx"))) {
    stop("your file must be a csv or an excel (xls or xlsx).")
  }

  if (not_in(sep, c(",", ";", "/t"))) {
    stop("In case of csv separator must be '/t', ';' or ','")
  }

  read_extension <- dplyr::case_when(
    file_extension == "csv" & sep == "," ~ "read.csv",
    file_extension == "csv" & sep == ";" ~ "read.csv2",
    .default = "read_excel"
  )

  if (file_extension == "csv") {
    file <- get(read_extension)(file, sep, header = TRUE, ...)
  } else {
    file <- get(read_extension, envir = asNamespace("readxl"))(file, ...)
  }

  return(file)
}

#' case_colname
#'
#' wrapper function that help modify column name to upper or lower case in order to be case unsensitive
#'
#' @param data dataset provided
#' @param case_type Only lower or upper cases are accepted.
#'
#' @author Romain Garreau
#' @export


case_colname <- function(data, case_type = c("lower", "upper")) {
  if (!is.data.frame(data)) stop("Error: data provided is not a dataframe")
  if (not_in(case_type, c("lower", "upper"))) stop("Error: Only 'lower' and 'upper' are supported")
  if (case_type == "lower") colnames(data) <- tolower(colnames(data))
  if (case_type == "upper") colnames(data) <- toupper(colnames(data))

  return(data)
}