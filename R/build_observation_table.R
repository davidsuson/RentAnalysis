#' build_observation_table
#'
#' Formats observation counts as a LaTeX table, with state-pair samples as
#' rows, the number of quarters, and dwelling/bedroom combinations as columns.
#' Non-control and control datasets are shown in separate panels.
#'
#' @param data A data frame with columns `State_Pair` (e.g. `"VIC_NSW"`),
#'   `Dwelling`, `Bedrooms`, `Observations`, `Quarters` and `Control`
#'   (`TRUE`/`FALSE`), with one row per
#'   Control x State_Pair x Dwelling x Bedrooms combination. Typically the
#'   `Observations` element of [build_LGA_and_observation_summary()].
#' @param pair_order Character vector giving the row order of state pairs
#'   within each panel.
#'
#' @returns A `kableExtra` LaTeX table.
#' @export
build_observation_table <- function(data,
                                     pair_order = c("VIC_NSW", "VIC_QLD", "VIC_SA")) {

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

  data <- data |>
    dplyr::select(Control, State_Pair, Dwelling, Bedrooms, Observations, Quarters) |>
    dplyr::filter(State_Pair %in% pair_order)

  quarters_by_sample <- dplyr::distinct(data, Control, State_Pair, Quarters)
  if (anyDuplicated(quarters_by_sample[c("Control", "State_Pair")])) {
    stop("`Quarters` differs across dwelling/bedroom combinations within a sample.")
  }

  wide <- data |>
    dplyr::inner_join(col_spec, by = c("Dwelling", "Bedrooms")) |>
    dplyr::select(Control, State_Pair, key, Observations) |>
    tidyr::pivot_wider(names_from = key, values_from = Observations) |>
    dplyr::left_join(quarters_by_sample, by = c("Control", "State_Pair")) |>
    dplyr::arrange(Control, factor(State_Pair, levels = pair_order)) |>
    dplyr::mutate(Sample = sub("_", " & ", State_Pair))

  # Panel row counts (FALSE sorts before TRUE, so non-control comes first)
  panels <- c(
    "Panel A: Without controls" = sum(!wide$Control),
    "Panel B: With controls"    = sum(wide$Control)
  )

  wide <- dplyr::select(wide, Sample, Quarters, dplyr::all_of(col_spec$key))

  # Spanning header: run lengths of consecutive groups
  groups <- rle(col_spec$group)
  header <- c(" " = 2, stats::setNames(groups$lengths, groups$values))

  tbl <- kableExtra::kbl(
    wide,
    format      = "latex",
    booktabs    = TRUE,
    col.names   = c("Sample", "Quarters", col_spec$label),
    align       = c("l", rep("r", nrow(col_spec) + 1)),
    format.args = list(big.mark = ",")
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
