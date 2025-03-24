#' rearrange_admin_data
#'
#' @param admin_data A tibble with the administration data
#' @return A tibble with the rearranged administration data
#' @export
#' @author Your Name
#'
#'

rearrange_admin_data <- function(admin_data) {
  rearranged_admin_data <- admin_data |>
    mutate(date = as.Date(admin_date), time = format(as.POSIXct(admin_date), "%H:%M")) |>
    select(-admin_date) |>
    dplyr::relocate(date, time, .before = route)

  return(rearranged_admin_data)
}

#' rearrange_tdm_data
#'
#' @param tdm_data A tibble with the TDM data
#' @return A tibble with the rearranged TDM data
#' @export
#' @author Your Name
#'

rearrange_tdm_data <- function(tdm_data) {
  rearranged_tdm_data <- tdm_data |>
    mutate(date = as.Date(tdm_date), time = format(as.POSIXct(tdm_date), "%H:%M")) |>
    select(-tdm_date) |>
    dplyr::relocate(date, time, .before = tdm_value)

  return(rearranged_tdm_data)
}


#' rearrange_weight_data
#'
#' @param weight_data A tibble with the weight data
#'  @return A tibble with the rearranged weight data
#' @export
#' @author Romain Garreau
#'

rearrange_weight_data <- function(weight_data) {
  rearranged_weight_data <- weight_data |>
    mutate(date = as.Date(weight_date)) |>
    select(-weight_date) |>
    dplyr::relocate(date, .before = weight_value)

  return(rearranged_weight_data)
}

#' create_pk_data
#'
#' @param admin_data A tibble with the administration data
#' @param tdm_data A tibble with the TDM data
#' @param weight_data A tibble with the weight data
#' @return A tibble with the PK data
#' @export
#' @author Romain Garreau
#'

create_pk_data <- function(admin_data, tdm_data, weight_data) {
  rearranged_admin_data <- rearrange_admin_data(admin_data)
  rearranged_tdm_data <- rearrange_tdm_data(tdm_data)
  rearranged_weight_data <- rearrange_weight_data(weight_data)

  pk_data <- full_join(rearranged_admin_data, rearranged_tdm_data, by = c("date", "time")) |>
    left_join(rearranged_weight_data, by = c("date")) |>
    arrange(date, time) |>
    rename(tbw = weight_value, observation = tdm_value, amount = dose)

  return(pk_data)
}

#' mb2_parse
#'
#' @param mb2_file a mb2_file
#' @return A tibble with the formatted PK data
#' @export
#' @author Romain Garreau
#'

mb2_parse <- function(mb2_file) {

  # read mb2 file
  mb2_info <- read_file.mb2(mb2_file)

  # create PK data
  pk_data <- create_pk_data(mb2_info$dose_df, mb2_info$level_df, mb2_info$weight_df)

  # bind additional columns
  pk_data <- pk_data |>
    mutate(
      id = 1,
      occasion = 1,
      event_id = ifelse(is.na(amount), 1, 4),
      height = mb2_info$height,
      birthdate = mb2_info$birthdate,
      #drug = mb2_info$drug,
      first_name = mb2_info$patient_first_name,
      last_name = mb2_info$patient_last_name
    ) |>
    dplyr::mutate(
      age = as.numeric(round((date - as.Date(birthdate)) / 365.25, digits = 0)),
      bmi = round((.data$tbw / (as.numeric(.data$height) / 100)^2), digits = 0)
    ) |>
    dplyr::select(-birthdate) |>
    dplyr::relocate(id, occasion, event_id, date, time, amount, route, infusion_rate, infusion_duration, observation, first_name, last_name, height, creatinin_clearance, tbw, age)

  return(pk_data)
}


# first_name = character(0),
#       last_name = character(0),
#       ipp = character(0),
#       id = integer(0),
#       occasion = integer(0),
#       time = numeric(0),
#       date = character(0),
#       event_id = integer(0),
#       observation = numeric(0),
#       amount = numeric(0),
#       infusion_duration = numeric(0),
#       birthdate = character(0),
#       weight = numeric(0),
#       height = numeric(0),
#       creatinine = numeric(0),
#       drug = character(0),
#       route = character(0) # Added route column
