#' pm2monolix
#'
#' 'pm2monolix' is a function intended to transform a pm file into a file readable by Monolix(R)
#'
#' @param input_file datafile
#' @param iv is drug given by iv route ? (TRUE or FALSE)
#' @param output_file name given to the file. default = "translate.csv"
#' @param pm_vers a numeric arugment that can be either 1 or 2. It refers to "old" or "new" Pmetrics version. Set to 1 by default (all Pmetrics version before 1.97)
#' @param ... additionnal argument that can be passed on read function (see utils::read.csv() and readxl::read_excel())
#'
#' @author Romain Garreau
#' @export


pm2monolix <- function(input_file = NULL,
                       iv = TRUE,
                       pm_vers = 1,
                       output_file = "pm2monolix.csv",
                       ...) {
  # test if input is one of 2 types :
  # - either a dataframe/tibble that inherits class object "pm" for pmetrics dataframe. this is obtained by using the read_pmetrics function
  # - a path to the file.  see ?PKtool::auto_read

  # test for condition descriibed above _______________________________________________________________________
  if (is.null(input_file)) stop("Error: no file provided")
  if (isFALSE(is.character(output_file))) stop("Error : output file name provided was not a character")
  if (not_in(pm_vers, c(1, 2))) stop("Error : pm_vers can only be 1 or 2. see description ?PKtool::pm2monolix")

  # if path to file this function use PKtool::read_pmetrics  ________________________________________________________
  if (!is.character(input_file) && attributes(input_file)$software == "pm.object") pm.data <- input_file
  if (is.character(input_file)) pm.data <- read_pmetrics(input_file, pm_vers = pm_vers, ...)

  # create new numerical ID specially for Pmetrics but add a column with matching previous ID.
  # for each unique ID (not case sensitive) create new ID (id, id.new )
  # column type
  pm.data <- case_colname(pm.data, "lower")

  lixoft.data <- pm.data |>
    dplyr::select(-c("c0", "c1", "c2", "c3")) |>
    dplyr::relocate("out",  .after = "dose") |>
    dplyr::relocate("evid", .after = "out")  |>
    dplyr::relocate("dur",  .after = "evid")

  col_name <- c(
    "ID" = "id",  "TIME" = "time",  "AMOUNT" = "dose",  "CONC" = "out",
    "EVENT_ID" = "evid",  "TINF" = "dur",  "ADDL" = "addl",  "II" = "ii",
    "ADMINISTRATION_ID" = "input",  "OBSERVATION_ID" = "outeq"
  )

  lixoft.data <- dplyr::rename(lixoft.data, dplyr::all_of(col_name)) |> case_colname("upper")

  # check if there is only one input and one observation (outeq) compartment
  if (length(unique(lixoft.data$ADMINISTRATION_ID)) < 3) lixoft.data <- dplyr::select(lixoft.data, -"ADMINISTRATION_ID")
  if (length(unique(lixoft.data$OBSERVATION_ID)) < 3) lixoft.data <- dplyr::select(lixoft.data, -"OBSERVATION_ID")

  # if iv = F remove the duration col
  # // if (isFALSE(iv)) {
  # //   lixoft.data <- lixoft.data |>
  # //     dplyr::select(-"TINF")
  # // }

  # output <- list(
  #   pmetrics_data = pm.data,
  #   monolix_data = lixoft.data
  # )

  # if (save) utils::write.csv(lixoft.data, file = output_file, row.names = FALSE) # save into csv (to replace later)

  return(lixoft.data)
}
