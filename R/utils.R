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

not_in <- function(a, b)  {
   ! a %in% b
}
