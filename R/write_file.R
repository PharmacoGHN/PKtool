#' @title write_file
#'
#' @description
#' this function create a file corresponding to the software for which the file was translated.
#' Only Pmetrics, Lixoft suite, PKsim and NONMEM type are supported. Software and extebsion type
#' are automatically retrvied from the object which must have been translated with the PKtool function.
#' Extention type is retrieved directly from the path given as the user must specify the path to save
#' the file by giving it name and extensiton ex : filename = "C:/newdata.csv" 
#'
#' @param object is the object supplied to write the file. Must be a dataframe or tibble
#' @param filename is the name the file should have
#' @param ... dictate the type of file created (only file supported by pharmacometrics software are possible)
#'
#' @author Romain Garreau
#' @export

write_file <- function(
  object = NULL,
  filename = NULL,
  ...
) {
  if (is.null(object)) stop("Error: there is no object to save.")
  if (is.null(filename)) stop("Error: give a the path where to save the file.")

  # check the extension passed is correct
  file_ext <- tools::file_ext(filename)
  if (not_in(file_ext, c("csv", "xlsx", "xls", "tsv"))) stop("This file extension is not supported. Please select file among csv, xlsx, xls or tsv")

  # check for software type
  software <- attributes(object)$software
  if (is.null(software)) stop("Error : file must come from a converting function")
  if (not_in(software, c("pm.object", "lixoft", "pksim", "nonmem", "mrgsolve"))) stop("Please select a software from Pmetrics, Lixoft, PKsim, or NONMEM.")

  if (software == "pm.object") {
    warning("Pmetrics files can only be saved as csv")
    filename <- stringr::str_replace(filename, paste0(".", file_ext), ".csv")
    # allow the use of pm_vers to add a column with #DEC_11 as in old version. pm_vers must get trough ... argument. default value will be 2
    utils::write.csv(object, file = filename, row.names = FALSE, sep = ",")
  }

  if (software == "nonmem") {
    warning("NONMEM files are only saved as csv")
    filename <- stringr::str_replace(filename, paste0(".", file_ext), ".csv")
    utils::write.csv(object, file = filename, row.names = FALSE, sep = ",")
  }
}
