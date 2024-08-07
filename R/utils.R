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
#' @export

not_in <- function(a, b) {
  !a %in% b
}


#' auto_read
#'
#' get the file extension and the separator values and call the adequate
#' function to read file.
#'
#' @noRd

auto_read <- function(file, sep = ",", ...) {
  file_extension <- tools::file_ext(file)

  # check file extension and sperator are valid.
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
