#' build_valid_LGA_table
#'
#' Finds every Excel file in `folder` whose name matches `outcome` and counts
#' the distinct LGAs with observations for each State x Dwelling x Bedrooms
#' combination in each file. It then combines the counts into one data frame
#' and formats them as a LaTeX table.
#'
#' Each file is labelled by its state pair, taken from the 4th and 5th
#' underscore-separated words of the file name. VIC appears in more than one
#' state-pair file, so VIC counts are taken from the `VIC_QLD` file only.
#'
#' @param folder A string. Path to the directory containing the Excel files.
#' @param outcome A string. Regular expression passed to
#'   `list.files(pattern = )` to select the files for one outcome
#'   (e.g. `"Median_Rent"`).
#'
#' @returns A list with two elements:
#'   \describe{
#'     \item{`latex_table`}{A `kableExtra` LaTeX table of valid LGA counts,
#'       with states as rows and dwelling/bedroom combinations as columns.}
#'     \item{`counts_table`}{A tibble with columns `State`, `Dwelling`,
#'       `Bedrooms` and `Valid_LGAs` (the number of distinct LGAs).}
#'   }
#'
#' @export
build_valid_LGA_table <- function(folder, outcome){

  file_paths <- list.files(folder, pattern = outcome, full.names = TRUE)
  state_pairs <- stringr::word(tools::file_path_sans_ext(basename(file_paths)), 4, 5, sep = "_")

  all_counts <- lapply(file_paths, count_unique_LGAs_by_state_dwelling_bedroom)
  names(all_counts) <- state_pairs
  all_counts_table <- all_counts |>
    dplyr::bind_rows(.id = "State_Pair") |>
    dplyr::filter(State != "VIC" | State == "VIC" & State_Pair == "VIC_QLD") |>
    dplyr::select(-State_Pair)
  # Only using counts for VIC from VIC_QLD.

  latex_table <- make_valid_LGA_table(all_counts_table)

  output <- list(
    latex_table = latex_table,
    counts_table = all_counts_table
  )

  return(output)

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

#' make_valid_LGA_table
#'
#' @param data A data frame with columns `State`, `Dwelling`, `Bedrooms` and
#'   `Valid_LGAs`.
#' @param state_order Character vector giving the row order of states.
#'
#' @returns A `kableExtra` LaTeX table.
make_valid_LGA_table <- function(data,
                                 state_order = c("VIC", "NSW", "SA", "QLD")) {

  # Dwelling x Bedrooms combinations -> table columns (row order = column order)
  col_spec <- tibble::tribble(
    ~Dwelling, ~Bedrooms, ~group,           ~label,
    "All Properties", "All Sizes", "All Properties", "All BR sizes",
    "Flat",    "1",       "Flat",           "1BR",
    "Flat",    "2",       "Flat",           "2BR",
    "Flat",    "3",       "Flat",           "3BR",
    "House",   "2",       "House",          "2BR",
    "House",   "3",       "House",          "3BR",
    "House",   "4",       "House",          "4BR"
  ) |>
    dplyr::mutate(key = paste(Dwelling, Bedrooms, sep = "_"))

  wide <- data |>
    dplyr::mutate(dplyr::across(c(Dwelling, Bedrooms), as.character)) |>
    dplyr::inner_join(col_spec, by = c("Dwelling", "Bedrooms")) |>
    dplyr::filter(State %in% state_order) |>
    dplyr::select(State, key, Valid_LGAs) |>
    tidyr::pivot_wider(names_from = key, values_from = Valid_LGAs) |>
    dplyr::arrange(factor(State, levels = state_order)) |>
    dplyr::select(State, dplyr::all_of(col_spec$key))

  # Spanning header: run lengths of consecutive groups
  groups <- rle(col_spec$group)
  header <- c(" " = 1, stats::setNames(groups$lengths, groups$values))

  kableExtra::kbl(
    wide,
    format    = "latex",
    booktabs  = TRUE,
    col.names = c("State", col_spec$label),
    align     = c("l", rep("r", nrow(col_spec)))
  ) |>
    kableExtra::add_header_above(header)
}
