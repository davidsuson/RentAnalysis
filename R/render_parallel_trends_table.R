#' render_parallel_trends_latex
#'
#' Renders the wide summary data frame produced by
#' `build_parallel_trends_table()` as a LaTeX table, splitting each state's
#' combined `"N = <n_obs>:nSig = <n_sig>"` string into separate `N_Obs` and
#' `N_Sig_Lag` columns grouped under a spanning header for that state.
#'
#' @param data A data frame as returned by `build_parallel_trends_table()`,
#'   with `Dwelling` and `Bedrooms` columns followed by one column per
#'   state, each cell containing a string of the form
#'   `"N = <n_obs>:nSig = <n_sig>"`.
#'
#' @returns A `kableExtra` LaTeX table object.
#' @export
render_parallel_trends_latex <- function(data) {

  states <- setdiff(names(data), c("Dwelling", "Bedrooms"))

  long_table <- data |>
    tidyr::pivot_longer(cols = dplyr::all_of(states),
                        names_to = "State",
                        values_to = "Summary") |>
    dplyr::mutate(
      Matches = stringr::str_match(Summary, "(?s)N\\s*=\\s*(\\d+).*?Sig\\s*=\\s*(\\d+)"),
      Obs = as.integer(Matches[, 2]),
      Sig = as.integer(Matches[, 3])
    ) |>
    dplyr::select(-Summary, -Matches)

  column_order <- as.vector(rbind(paste0(states, "Obs"), paste0(states, "Sig")))

  wide_table <- long_table |>
    tidyr::pivot_wider(
      id_cols = c(Dwelling, Bedrooms),
      names_from = State,
      values_from = c(Obs, Sig),
      names_glue = "{State}{.value}"
    ) |>
    dplyr::relocate(Dwelling, Bedrooms, dplyr::all_of(column_order))

  # This will be the state headers. Have to add manually since the column names
  # are used to denote the number of observations and significant lag coeffs.

  header <- c(" " = 2, stats::setNames(rep(2, length(states)), states))
  kableExtra::kbl(
    wide_table,
    format = "latex",
    booktabs = TRUE,
    col.names = c("Dwelling", "Bedrooms", rep(c("Obs", "Sig"), length(states)))
  ) |>
    kableExtra::add_header_above(header)
}
