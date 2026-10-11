#' filter_state_panel_and_complete_observations
#'
#' Restricts the data to the quarters from `starting_year` Q`starting_quarter`
#' to `max_year` Q`max_quarter` (inclusive), adds a running quarter index with
#' [add_running_quarters()], and pads each LGA's panel with
#' [complete_LGA_observations()] so it has a row for every quarter in that
#' range. Rows added for missing quarters get `NA` in all columns except the
#' identifiers, `Running_Quarter`, `Year` and `Quarter`.
#'
#' @param data A data frame with columns `State`, `Dwelling`, `Bedrooms`,
#'   `LGA`, `Year` and `Quarter` (numeric, 1--4).
#' @param max_year An integer. Year of the last quarter to keep.
#' @param max_quarter An integer (1--4). Quarter of the last quarter to keep.
#' @param starting_year An integer. Year of the first quarter to keep.
#'   Defaults to 2018.
#' @param starting_quarter An integer (1--4). Quarter of the first quarter to
#'   keep. Defaults to 1.
#'
#' @returns `data` restricted to the chosen range, with a `Running_Quarter`
#'   column (1 = the starting quarter) and one row per
#'   State x Dwelling x Bedrooms x LGA x quarter.
filter_state_panel_and_complete_observations <- function(data, max_year, max_quarter, starting_year = 2018, starting_quarter = 1){

  max_running_quarter <- (max_year - starting_year) * 4 + (max_quarter - starting_quarter) + 1

  complete_data <- data |>
    dplyr::filter(Year < max_year | (Year == max_year & Quarter <= max_quarter),
                  Year > starting_year | (Year == starting_year & Quarter >= starting_quarter)) |>
    add_running_quarters() |>
    complete_LGA_observations(max_running_quarter = max_running_quarter) |>
    dplyr::mutate(
      Quarter_Start_Date = dplyr::coalesce(
        Quarter_Start_Date,
        lubridate::make_date(Year, Quarter * 3 - 2, 1)
      ),
      Quarter_End_Date = dplyr::coalesce(
        Quarter_End_Date,
        lubridate::make_date(Year + (Quarter == 4), (Quarter %% 4) * 3 + 1, 1) - 1
      )
    )

  complete_data
}
