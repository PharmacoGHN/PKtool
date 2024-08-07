#' @title write_file
#'
#' @param object is the object supplied to write the file. Must be a dataframe or tibble
#' @param filename is the name the file should have
#' @param software correspond to the software for which the file is adapted. Only Pmetrics, Lixoft suite, PKsim and NONMEM are supported
#' @param ext dictate the type of file created
#' (only file supported by pharmacometrics software are possible)

write_file <- function(
      object,
      filename,
      software = c("Pmetrics", "Lixoft", "PKsim", "NONMEM"),
      ext = c("csv", "xlsx", "xls", "tsv")) {

   if (length(software) > 1 || length(ext) > 1) {
      rlang::abort("Error : please check that only one object was passed to software and ext argument")
   }

   if (not_in(software, c("Pmetrics", "Lixoft", "PKsim", "NONMEM"))) {
      rlang::abort(message = "Please select a software from Pmetrics, Lixoft, PKsim, or NONMEM. The other software are not supported")
   }

   if (not_in(ext, c("csv", "xlsx", "xls", "tsv"))) {
      rlang::abort(message = "This file extension is not supported. Please select file among csv, xlsx, xls or tsv")
   }

   if (software == "Pmetrics") {
      utils::write.csv2(object, file = file, row.names = FALSE, sep = ",")
   }
}