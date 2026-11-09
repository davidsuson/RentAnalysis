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
#'   `"att_se"` (aggregate ATT with significance stars: * p<0.10,
#'   ** p<0.05, *** p<0.01) or `"pretest_p"` (parallel trends pre-test
#'   p-value).
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

  wide <- dplyr::bind_rows(
    flatten_results(results$non_control, control = FALSE, value = value, digits = digits),
    flatten_results(results$control,     control = TRUE,  value = value, digits = digits)
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

  tbl <- kableExtra::kbl(
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

  # Significance note under the ATT table
  if (value == "att_se") {
    tbl <- kableExtra::footnote(
      tbl,
      general        = "* p<0.10, ** p<0.05, *** p<0.01",
      general_title  = "",
      threeparttable = TRUE
    )
  }

  tbl
}


# ---- Helpers ----------------------------------------------------------------

#' Flatten state pair -> sheet -> value into one row per model
#'
#' @param panel_results One panel of [run_did_regressions()] output
#'   (`non_control` or `control`).
#' @param control Logical. Whether this panel used controls.
#' @param value A string. `"att_se"` or another element name, e.g. `"pretest_p"`.
#' @param digits Integer. Decimal places for numeric values.
#'
#' @returns A tibble with columns `State_Pair`, `key`, `Value`, `Control`.
#' @noRd
flatten_results <- function(panel_results, control, value, digits = 3) {
  purrr::imap(panel_results, function(sheets, pair) {
    tibble::tibble(
      State_Pair = pair,
      key        = names(sheets),
      # att_se shows ATT + significance stars instead of the SE string
      Value      = purrr::map_chr(sheets, function(s) {
        if (value == "att_se") format_att_stars(s, digits) else format_value(s[[value]], digits)
      })
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(Control = control)
}

#' Format one value: numbers get fixed decimals, strings pass through
#'
#' @param x A number or string.
#' @param digits Integer. Decimal places for numeric values.
#'
#' @returns A string.
#' @noRd
format_value <- function(x, digits = 3) {
  if (is.numeric(x)) formatC(x, format = "f", digits = digits) else as.character(x)
}

#' Format an ATT with significance stars
#'
#' Two-sided z-test on att / se: *** p<0.01, ** p<0.05, * p<0.10.
#'
#' @param sheet One sheet's results from [summarise_did()], with `att` and `se`.
#' @param digits Integer. Decimal places for the ATT.
#'
#' @returns A string such as `"12.345**"`, or `NA` if att/se is missing.
#' @noRd
format_att_stars <- function(sheet, digits = 3) {
  att <- sheet$att
  se  <- sheet$se
  if (is.null(att) || is.null(se) || is.na(att) || is.na(se) || se == 0) {
    return(NA_character_)
  }
  p     <- 2 * stats::pnorm(-abs(att / se))
  stars <- if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
  paste0(formatC(att, format = "f", digits = digits), stars)
}
