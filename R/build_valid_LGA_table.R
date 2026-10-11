#' build_valid_LGA_table
#'
#' Formats counts of valid LGAs as a LaTeX table, with states as rows,
#' dwelling/bedroom combinations as columns, and separate panels for the
#' non-control and control datasets.
#'
#' @param data A data frame with columns `Control` (`TRUE`/`FALSE`), `State`,
#'   `Dwelling`, `Bedrooms` and `Valid_LGAs`.
#' @param state_order Character vector giving the row order of states within
#'   each panel.
#'
#' @returns A `kableExtra` LaTeX table.
#' @export
build_valid_LGA_table <- function(data,
                                  state_order = c("VIC", "NSW", "SA", "QLD")) {

  # Dwelling x Bedrooms combinations -> table columns (row order = column order)
  col_spec <- tibble::tribble(
    ~Dwelling,        ~Bedrooms,   ~group,           ~label,
    "All Properties", "All Sizes", "All Properties", "All BR sizes",
    "Flat",           "1",         "Flat",           "1BR",
    "Flat",           "2",         "Flat",           "2BR",
    "Flat",           "3",         "Flat",           "3BR",
    "House",          "2",         "House",          "2BR",
    "House",          "3",         "House",          "3BR",
    "House",          "4",         "House",          "4BR"
  ) |>
    dplyr::mutate(key = paste(Dwelling, Bedrooms, sep = "_"))

  wide <- data |>
    dplyr::select(Control, State, Dwelling, Bedrooms, Valid_LGAs) |>
    dplyr::mutate(
      Control = as.logical(Control),
      dplyr::across(c(Dwelling, Bedrooms), as.character)
    ) |>
    dplyr::inner_join(col_spec, by = c("Dwelling", "Bedrooms")) |>
    dplyr::filter(State %in% state_order) |>
    dplyr::select(Control, State, key, Valid_LGAs) |>
    tidyr::pivot_wider(names_from = key, values_from = Valid_LGAs) |>
    dplyr::arrange(Control, factor(State, levels = state_order))

  # Panel row counts (FALSE sorts before TRUE, so non-control comes first)
  panels <- c(
    "Panel A: Without controls" = sum(!wide$Control),
    "Panel B: With controls"    = sum(wide$Control)
  )

  wide <- dplyr::select(wide, State, dplyr::all_of(col_spec$key))

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
    kableExtra::add_header_above(header) |>
    kableExtra::pack_rows(
      index             = panels,
      bold              = FALSE,
      italic            = TRUE,
      indent            = FALSE,
      latex_gap_space   = "0.5em",
      extra_latex_after = "\\addlinespace[2pt]"
    )
}

