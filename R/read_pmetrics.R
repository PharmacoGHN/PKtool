#' read_pmetrics
#'
#' @description
#' this function purpose is to read and extract data from a pmetrics file.
#' It is the first step before any translation of a Pmetrics type file into
#' another software type.
#'
#' @param data is the name of the file to read (csv, xlsx and xls file supported)
#' @param pm_vers correspond to the version of Pmetrics. Can take either 1 for all version before the 2.0.0 or 2 for all version above the 2.0.0
#' @param ... additionnal argument that can be passed on read function (see utils::read.csv() and readxl::read_excel())
#'
#' @author Romain Garreau
#' @export


read_pmetrics <- function(data, pm_vers = NULL, ...) {
  # set the default to legacy Pmetrics version
  if (is.null(pm_vers)) {
    rlang::warn(message = "Warning: the version of Pmetrics was not provided and was set to < 2.0.0 by default.")
    pm_vers <- 1
  }

  skip <- ifelse(pm_vers == 1, 1, 0) # load csv file and skip the first row if pmetrics ver < 2 (POPDATA DEC_11)
  pm_data <- auto_read(data, skip = skip, sep = ";")

  # if csv file with semi colon delimiters, read.csv will return a file with ncol == 1
  if (length(pm_data) == 1) {
    pm_data <- auto_read(data, skip = skip)
  }

  # Check required variable
  file_info <- pm_check_colname(pm_data)
  if (!file_info$check$mandatory) stop("Some mandatory variable are missing. Please check that 'ID', 'TIME', 'DUR', 'DOSE' and 'OUT' are present.")

  # add check to verify colname, if number, return pm_vers = 2 and reload file

  # add missing column that are not mandatory in Pmetrics V2 and add covar if existing
  if (!file_info$check$complete) {
    pm_data <- pm_autocomplete(pm_data, file_info$var$missing, file_info$covar$name)
  }

  return(pm_data)
}




#' pm_check_colname
#'
#' @description
#' function that check if the minimum colnames required to used Pmetrics are here.
#'
#' @param data is the data in which the colum names must be checked
#'
#' @author Romain Garreau
#' @export

pm_check_colname <- function(data) {

  pm_colnames <- gsub("x.|#", "", tolower(colnames(data))) # remove the "x." created because of "#" in old Pmetrics files
  expected_pm_colnames <- c("id", "evid", "time", "dur", "dose", "addl", "ii", "input", "out", "outeq", "c0", "c1", "c2", "c3")

  # extract existing variable *var_name*
  var_pos <- which(pm_colnames %in% expected_pm_colnames)
  var_names <- pm_colnames[var_pos]
  var_missing <- expected_pm_colnames[which(not_in(expected_pm_colnames, pm_colnames))]

  # check if the 5 mandatory variable are present. NB : the check is not case sensitive
  mandatory_pm_var <- c("id", "time", "dur", "dose", "out")
  is_mandatory <- ifelse(all(mandatory_pm_var %in% var_names), TRUE, FALSE)

  # check if covariate are present and get their position
  cov_pos <- which(not_in(pm_colnames, expected_pm_colnames))
  cov_name <- pm_colnames[cov_pos]

  # check if the data is complete (all expected colnames are present)
  is_complete <- ifelse(all(expected_pm_colnames %in% pm_colnames), TRUE, FALSE)

  return(
    list(
      check = list(mandatory = is_mandatory, complete = is_complete),
      var = list(name = var_names, pos = var_pos, missing = var_missing),
      covar = list(name = cov_name, pos = cov_pos)
    )
  )

}



#' pm_autocomplete
#'
#' @description
#' this function will add the necessary covariate in order to translate an incomplete Pmetrics file (Version 2 only)
#'
#' @param data data from Pmetrics
#' @param missing_var missing variable. See ?PKtool::pm_check_colname
#' @param cov correspond to the covariate present in the dataset
#'
#' @author Romain Garreau
#' @export


pm_autocomplete <- function(data, missing_var, cov = NULL) {

  colnames(data) <- tolower(colnames(data))

  if (!identical(cov, character(0))) {
    covariate <- dplyr::select(data, dplyr::all_of(cov))
    data <- dplyr::select(data, -dplyr::all_of(cov))
  }

  if ("evid" %in% missing_var) { data <- dplyr::mutate(data, evid = ifelse(data$dose == ".", 0, 1))}
  if ("addl" %in% missing_var) { data <- dplyr::mutate(data, addl = ".")}
  if ("ii" %in% missing_var) { data <- dplyr::mutate(data, ii = ".")}
  if ("input" %in% missing_var) { data <- dplyr::mutate(data, input = ifelse(data$dose == ".", ".", "1"))}
  if ("outeq" %in% missing_var) { data <- dplyr::mutate(data, outeq = ifelse(data$out == ".", ".", "1"))}
  if ("c0" %in% missing_var) { data <- dplyr::mutate(data, c0 = ".")}
  if ("c1" %in% missing_var) { data <- dplyr::mutate(data, c1 = ".")}
  if ("c2" %in% missing_var) { data <- dplyr::mutate(data, c2 = ".")}
  if ("c3" %in% missing_var) { data <- dplyr::mutate(data, c3 = ".")}

  # relocate data
  data <- data |>
    dplyr::relocate("evid", .after = "id") |>
    dplyr::relocate("out", .after = "input")

  if (!identical(cov, character(0))) {
    data <- data |>
      dplyr::bind_cols(covariate)
  }

  colnames(data) <- toupper(colnames(data))

  return(as.data.frame(data))
}


# add read_monolix ; read_pksim; read_nonmem