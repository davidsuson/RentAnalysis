#' complete_LGA_observations
#'
#' Pads each LGA's panel so it has a row for every quarter from 1 to
#' `max_quarter`, within each State x Dwelling x Bedrooms combination. Rows
#' added for missing quarters get `NA` in all other columns, except `Year`
#' and `Quarter`.
#'
#' @param data A data frame with columns `State`, `Dwelling`, `Bedrooms`,
#'   `LGA`, `Running_Quarter`, `Year` and `Quarter`.
#' @param max_quarter An integer. The last `Running_Quarter` each LGA should
#'   have. 2018 Q1 has the value of 1 as the first running_quarter.
#'
#' @returns `data` with one row per State x Dwelling x Bedrooms x LGA x
#'   quarter, from 1 to `max_quarter`.
#'
#' @keywords internal
complete_LGA_observations <- function(data, max_running_quarter){

  complete_data <- data |>
    tidyr::complete(
      tidyr::nesting(State, Dwelling, Bedrooms, Region, LGA),
      Running_Quarter = seq_len(max_running_quarter)
    ) |>
    add_year_quarter()

}

# HELPERS -----------------------------------------------------------------

#' add_year_quarter
#'
#' Fills in missing `Year` and `Quarter` values from `Running_Quarter`,
#' starting 2018 Q1. Existing values are left unchanged.
#'
#' @param data A data frame with columns `Running_Quarter`, `Year` and
#'   `Quarter` (numeric, 1--4).
#'
#' @returns `data` with missing `Year` and `Quarter` values filled in.
add_year_quarter <- function(data) {

  START_YEAR = 2018
  START_QUARTER = 1

  start_idx <- START_YEAR * 4 + (START_QUARTER - 1)

  data |>
    dplyr::mutate(
      idx     = start_idx + Running_Quarter - 1,
      Year    = dplyr::coalesce(Year, idx %/% 4),
      Quarter = dplyr::coalesce(Quarter, idx %% 4 + 1)
    ) |>
    dplyr::select(-idx)
}
