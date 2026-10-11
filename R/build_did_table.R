#' build_did_table
#'
#' Formats one result from [run_did_regressions()] as a LaTeX table, with
#' state-pair samples as rows and dwelling/bedroom combinations as columns.
#' Non-control and control results are shown in separate panels.
#'
#' @param results The list returned by [run_did_regressions()], with elements
#'   `non_control` and `control`, each a list of state pairs containing a list
#'   of sheets.
#' @param value A string. Which element of each sheet's results to show:
#'   `"att_se"` (aggregate ATT with SE in brackets) or `"pretest_p"`
#'   (parallel trends pre-test p-value).
#' @param digits Integer. Decimal places for numeric values. Ignored for
#'   values that are already strings.
#' @param pair_order Character vector giving the row order of state pairs
#'   within each panel.
#'
#' @returns A `kableExtra` LaTeX table.
#' @export
build_did_table <- function(results,
                            value = "att_se",
                            digits = 3,
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

  # Format one value: numbers get fixed decimals, strings pass through
  format_value <- function(x) {
    if (is.numeric(x)) formatC(x, format = "f", digits = digits) else as.character(x)
  }

  # Flatten state pair -> sheet -> value into one row per model
  flatten_results <- function(panel_results, control) {
    purrr::imap(panel_results, function(sheets, pair) {
      tibble::tibble(
        State_Pair = pair,
        key        = names(sheets),
        Value      = purrr::map_chr(sheets, ~ format_value(.x[[value]]))
      )
    }) |>
      dplyr::bind_rows() |>
      dplyr::mutate(Control = control)
  }

  wide <- dplyr::bind_rows(
    flatten_results(results$non_control, FALSE),
    flatten_results(results$control,     TRUE)
  ) |>
    dplyr::filter(State_Pair %in% pair_order, key %in% col_spec$key) |>
    tidyr::pivot_wider(names_from = key, values_from = Value) |>
    dplyr::arrange(Control, factor(State_Pair, levels = pair_order)) |>
    dplyr::mutate(Sample = sub("_", " & ", State_Pair))

  # Panel row counts (FALSE sorts before TRUE, so non-control comes first)
  panels <- c(
    "Panel A: Without controls" = sum(!wide$Control),
    "Panel B: With controls"    = sum(wide$Control)
  )

  wide <- dplyr::select(wide, Sample, dplyr::all_of(col_spec$key))

  # Spanning header: run lengths of consecutive groups
  groups <- rle(col_spec$group)
  header <- c(" " = 1, stats::setNames(groups$lengths, groups$values))

  kableExtra::kbl(
    wide,
    format    = "latex",
    booktabs  = TRUE,
    col.names = c("Sample", col_spec$label),
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
    ) |>
    kableExtra::row_spec(0, align = "c")
}
