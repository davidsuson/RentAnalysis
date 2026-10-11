#' build_LGA_and_observation_summary
#'
#' Builds a combined summary of valid LGAs, observations and quarters for one
#' rent outcome, covering both the non-control and control datasets. Each
#' dataset is summarised with [load_cleaned_data_folder()], labelled with a
#' `Control` column, and stacked.
#'
#' @param non_control_folder A string. Path to the directory containing the
#'   Excel files for the non-control time periods.
#' @param control_folder A string. Path to the directory containing the Excel
#'   files for the control time periods.
#' @param outcome A string. Regular expression passed to
#'   `list.files(pattern = )` to select the files for one outcome
#'   (e.g. `"Median_Rent"`).
#'
#' @returns A tibble with one row per Control x State x Dwelling x Bedrooms
#'   combination and the columns:
#'   \describe{
#'     \item{`State`, `Dwelling`, `Bedrooms`}{The grouping variables.}
#'     \item{`Valid_LGAs`}{Integer. The number of distinct LGAs.}
#'     \item{`Observations`}{Integer. The number of LGA--quarter
#'       observations.}
#'     \item{`Quarters`}{Integer. The number of quarters.}
#'     \item{`Control`}{Logical. `TRUE` for the control dataset, `FALSE`
#'       otherwise.}
#'   }
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

#' summarise_by_state_dwelling_bedroom
#'
#' Reads every sheet of an Excel workbook, stacks them into one data frame,
#' and summarises the data for each State x Dwelling x Bedrooms combination:
#' the number of distinct LGAs, the number of observations, and the number
#' of quarters.
#'
#' @param file_path A string. Path to an `.xlsx`/`.xls` file. Every
#'   sheet must contain the columns `State`, `Dwelling`, `Bedrooms`, `LGA`
#'   and `Running_Quarters`.
#'
#' @returns A tibble with one row per State x Dwelling x Bedrooms
#'   combination and the columns:
#'   \describe{
#'     \item{`State`, `Dwelling`, `Bedrooms`}{The grouping variables.}
#'     \item{`Valid_LGAs`}{Integer. The number of distinct LGAs.}
#'     \item{`Observations`}{Integer. The number of rows (LGA--quarter
#'       observations).}
#'     \item{`Quarters`}{Integer. The number of distinct quarters.}
#'   }
summarise_by_state_dwelling_bedroom <- function(file_path){

  binded_data <- readxl::excel_sheets(file_path) |>
    purrr::set_names() |>
    purrr::map(~ readxl::read_excel(file_path, sheet = .x)) |>
    dplyr::bind_rows()

  problems <- check_complete_panels(binded_data)
  stopifnot(nrow(problems) == 0)

  counts <- binded_data |>
    dplyr::group_by(State, Dwelling, Bedrooms) |>
    dplyr::summarise(Valid_LGAs = dplyr::n_distinct(LGA),
                     Observations = dplyr::n(),
                     Quarters = max(Running_Quarter),
                     .groups = "drop")

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

  all_counts <- lapply(file_paths, summarise_by_state_dwelling_bedroom)
  names(all_counts) <- state_pairs
  all_counts_table <- all_counts |>
    dplyr::bind_rows(.id = "State_Pair") |>
    dplyr::filter(State != "VIC" | State == "VIC" & State_Pair == "VIC_QLD") |>
    dplyr::select(-State_Pair)
  # Only using counts for VIC from VIC_QLD.

}

#' check_complete_panels
#'
#' Checks that every LGA has a complete, gap-free run of quarters from 1 to
#' the maximum `Running_Quarters`, within each State x Dwelling x Bedrooms
#' combination.
#'
#' @param data A data frame with columns `State`, `Dwelling`, `Bedrooms`,
#'   `LGA` and `Running_Quarters`.
#'
#' @returns A tibble of LGAs with incomplete paths (zero rows if all are
#'   complete), with columns describing the problem:
#'   \describe{
#'     \item{`n_rows`}{Number of rows for the LGA.}
#'     \item{`n_quarters`}{Number of distinct quarters.}
#'     \item{`first_q`, `last_q`}{First and last quarter observed.}
#'     \item{`max_q`}{The maximum quarter in the whole dataset.}
#'     \item{`missing_q`}{The quarters that are missing, as a string.}
#'   }
check_complete_panels <- function(data) {

  max_q <- max(data$Running_Quarter, na.rm = TRUE)

  data |>
    dplyr::group_by(State, Dwelling, Bedrooms, LGA) |>
    dplyr::summarise(
      n_rows     = dplyr::n(),
      n_quarters = dplyr::n_distinct(Running_Quarter),
      first_q    = min(Running_Quarter),
      last_q     = max(Running_Quarter),
      missing_q  = toString(setdiff(seq_len(max_q), Running_Quarter)),
      .groups    = "drop"
    ) |>
    dplyr::mutate(max_q = max_q) |>
    dplyr::filter(
      n_quarters != max_q |   # doesn't cover every quarter (gaps, late start, early end)
        n_rows != n_quarters    # duplicated quarters
    )
}
