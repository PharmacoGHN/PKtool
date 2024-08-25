# Description
#
# This is a set of function designed to read PKsim file and perform certain check before
# Changing the data into another format.

#' read_psim
#'
#' read file coming from PKsim and decompose important feature to translate into other files.
#'
#' @param data
#'
#' @author Romain Garreau
#' @noRd



read_psim <- function(data) {
  # attr(psim_data, "software") <- "pksim"
  # return(psim_data)
}


#' decompose_psim
#'
#' function that decompose complexe PKsim file
#'
#' @param data datafile
#'
#' @author Romain Garreau
#' @noRd


decompose_psim <- function(data) {
  # decompose compartment with unique and similar name with string distance, return a warning
  # do this for ID aswell
  # remove units in header if present, use regex to find them. In general these are inclosed in bracket : [mg/L], see pksim nomenclature
  # store eache variable in a list objet to recompose new dataframe from it.
  # find a way to handle occasion for monolix
}