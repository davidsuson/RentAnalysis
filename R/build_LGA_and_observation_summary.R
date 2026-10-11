#' build_LGA_and_observation_summary
#'
#' Builds a combined table of valid LGA counts for one rent outcome, covering
#' both the non-control and control datasets. Labelled with a `Control`
#' column before being stacked.
#'
#' @param non_control_folder A string. Path to the directory containing the
#'   Excel files for the non-control time periods.
#' @param control_folder A string. Path to the directory containing the Excel
#'   files for the control time periods.
#' @param outcome A string. Regular expression passed to
#'   `list.files(pattern = )` to select the files for one outcome
#'   (e.g. `"Median_Rent"`).
#'
#' @returns A tibble with columns `State`, `Dwelling`, `Bedrooms`,
#'   `Valid_LGAs` (the number of distinct LGAs) and `Control` (`TRUE` for the
#'   control dataset, `FALSE` otherwise).
#'
#' @export
build_LGA_and_observation_summary <- function(non_control_folder,control_folder, outcome){

  non_control_summary <- load_cleaned_data_folder(non_control_folder, outcome) |>
    dplyr::mutate(Control = FALSE)

  control_summary <- load_cleaned_data_folder(control_folder, outcome) |>
    dplyr::mutate(Control = TRUE)

  summary_table <- dplyr::bind_rows(non_control_summary, control_summary)

}



# HELPERS  -----------------------------------------------------------

#' count_unique_LGAs_by_state_dwelling_bedroom
#'
#' Reads every sheet of an Excel workbook, stacks them into one data frame,
#' and counts the distinct LGAs for each State x Dwelling x Bedrooms
#' combination.
#'
#' @param file_path A string. Path to an `.xlsx`/`.xls` file. Every
#'   sheet must contain the columns `State`, `Dwelling`, `Bedrooms` and `LGA`.
#'
#' @returns A tibble with one row per State x Dwelling x Bedrooms
#'   combination and the columns `State`, `Dwelling`, `Bedrooms` and
#'   `Valid_LGAs` (integer: the number of distinct LGAs).
count_unique_LGAs_by_state_dwelling_bedroom <- function(file_path){

  counts <- readxl::excel_sheets(file_path) |>
    purrr::set_names() |>
    purrr::map(~ read_excel(file_path, sheet = .x)) |>
    dplyr::bind_rows() |>
    dplyr::group_by(State, Dwelling, Bedrooms) |>
    dplyr::summarise(Valid_LGAs = dplyr::n_distinct(LGA), .groups = "drop")

}

#' load_cleaned_data_folder
#'
#' Counts valid LGAs for every Excel file in a folder that matches `outcome`,
#' and combines the counts into one data frame.
#'
#' Each file is labelled by its state pair, taken from the 4th and 5th
#' underscore-separated words of the file name. VIC appears in every
#' state-pair file, so VIC counts are taken from the `VIC_QLD` file only.
#'
#' @param folder_path A string. Path to the directory containing the Excel
#'   files.
#' @param outcome A string. Regular expression passed to
#'   `list.files(pattern = )` to select the files for one outcome
#'   (e.g. `"Median_Rent"`).
#'
#' @returns A tibble with one row per State x Dwelling x Bedrooms
#'   combination and the columns `State`, `Dwelling`, `Bedrooms` and
#'   `Valid_LGAs`.
load_cleaned_data_folder <- function(folder_path, outcome){

  file_paths <- list.files(folder_path, pattern = outcome, full.names = TRUE)
  state_pairs <- stringr::word(tools::file_path_sans_ext(basename(file_paths)), 4, 5, sep = "_")

  all_counts <- lapply(file_paths, count_unique_LGAs_by_state_dwelling_bedroom)
  names(all_counts) <- state_pairs
  all_counts_table <- all_counts |>
    dplyr::bind_rows(.id = "State_Pair") |>
    dplyr::filter(State != "VIC" | State == "VIC" & State_Pair == "VIC_QLD") |>
    dplyr::select(-State_Pair)
  # Only using counts for VIC from VIC_QLD.

}
