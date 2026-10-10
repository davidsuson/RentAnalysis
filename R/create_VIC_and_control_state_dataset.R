#' create_vic_and_control_state_dataset
#'
#' Filters the data to Victoria and a single control state, truncates both to
#' the control state's last available quarter, and splits the result into one
#' data frame per dwelling type and bedroom count combination.
#'
#' @param data A data.frame. Any sheet loaded from combined_dataset_no_controls.xlsx or combined_dataset_with_controls.xlsx.
#' @param control_state A string. Control state abbreviation, e.g. `"NSW"`, `"QLD"` or `"SA"`.
#' @param outcome A string. The outcome column to keep, either `"Median Rent"`
#'   or `"New Bonds"`. The other is dropped.
#'
#' @returns A named list of data frames, one per dwelling-bedroom combination,
#'  named as `"Dwelling_Bedrooms"` (e.g. `"Flat_2"`).
#' @export
create_vic_and_control_state_dataset <- function(data, control_state, outcome) {
  control_state_data <- data |>
    dplyr::filter(State == control_state)

  control_state_max_year <- max(control_state_data$Year)
  control_state_max_quarter <- control_state_data |>
    dplyr::filter(Year == control_state_max_year) |>
    dplyr::pull(Quarter) |>
    max()

  filtered_data <- data |>
    dplyr::filter(
      State %in% c("VIC", control_state),
      Year < control_state_max_year |
        (
          Year == control_state_max_year &
            Quarter <= control_state_max_quarter
        )
    )

  grouped_filtered_data <- filtered_data |>
    dplyr::group_by(Dwelling, Bedrooms)

  dwelling_bedroom_combos <- grouped_filtered_data  |>
    dplyr::group_split()

  names(dwelling_bedroom_combos) <- grouped_filtered_data |>
    dplyr::group_by(Dwelling, Bedrooms) |>
    dplyr::group_keys() |>
    tidyr::unite("name", Dwelling, Bedrooms) |>
    dplyr::pull(name)

  if ("LGA" %in% names(data)) {
    # If a LGA has one or more NAs in the outcome variable, remove the LGA entirely.
    dwelling_bedroom_combos <- purrr::map(
      dwelling_bedroom_combos,
      \(combo) combo |>
        dplyr::group_by(LGA) |>
        dplyr::filter(!any(is.na(
          !!rlang::sym(outcome)
        ))) |>
        dplyr::ungroup()
    )
  }


  all_outcomes <- c("Median_Rent", "New_Bonds")

  final_dataset <- purrr::map(dwelling_bedroom_combos,
                              \(combo) combo |>
                                dplyr::select(-dplyr::any_of(setdiff(
                                  all_outcomes, outcome
                                ))))

  return(final_dataset)
}
