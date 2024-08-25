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
  if(is.null(object)) stop("Error: there is no object to save.")
  if(is.null(filename)) stop("Error: give a the path where to save the file.")
  
  # check the extension passed is correct
  file_ext = tools::file_ext(filename)
  if (not_in(file_ext, c("csv", "xlsx", "xls", "tsv"))) stop("This file extension is not supported. Please select file among csv, xlsx, xls or tsv")
  
  # check for software type
  software <- attributes(object)$software
  if (not_in(software, c("Pmetrics", "Lixoft", "PKsim", "NONMEM"))) stop("Please select a software from Pmetrics, Lixoft, PKsim, or NONMEM. The other software are not supported")
    
  
  if (software == "Pmetrics") {
    utils::write.csv(object, file = filename, row.names = FALSE, sep = ",")
  }
}
