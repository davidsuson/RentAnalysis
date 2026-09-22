#' build_parallel_trends_table
#'
#' Given a named list of fitted `fixest` models with each item corresponding to a
#' dwelling type / bedroom count / state combination, the function builds a wide summary
#' table with one row per dwelling/bedroom combination and one column per
#' state. Each cell reports the number of observations and the number
#' of significant pre-treatment ("lag") coefficients for that regression.
#'
#' @param regressions A named list of fitted `fixest` model objects. Each
#'   name must follow the pattern `"<dwelling>_<bedroom>_<state>"` (e.g.
#'   `"House_2_NSW"`), matching the underscore-delimited structure expected
#'   by `stringr::str_split_fixed(..., "_", 3)`.
#' @param alpha Numeric. The significance level passed through to
#'   `extract_model_details()` for flagging significant pre-treatment
#'   coefficients. Defaults to `0.05`.
#' @param pre_threshold Numeric. The relative-time cutoff passed through to
#'   `extract_model_details()` for identifying pre-treatment ("lag")
#'   coefficients. Defaults to `-1`.
#'
#' @returns A data frame with one row per unique dwelling/bedroom
#'   combination and one column per state, plus leading `Dwelling` and
#'   `Bedrooms` columns identifying each row. Each state column holds a
#'   character string of the form `"N = <n_obs>:nSig = <n_sig>"`
#'   summarising that dwelling/bedroom/state regression.
#' @export
build_parallel_trends_table <- function(regressions,
                                        alpha = 0.05,
                                        pre_threshold = -1) {

  row_labels <- stringr::str_split_fixed(names(regressions), "_", 3) |>
    as.data.frame(stringsAsFactors = FALSE) |>
    setNames(c("dwelling", "bedroom", "control"))

  row_keys <- unique(row_labels[c("dwelling", "bedroom")])
  states <- sort(unique(row_labels$control))

  summary_matrix <- matrix(NA_character_,
                           nrow = nrow(row_keys),
                           ncol = length(states),
                           dimnames = list(NULL, states))

  for (row in seq_len(nrow(row_keys))) {
    for (state in seq_along(states)) {

      key <- paste(row_keys$dwelling[row], row_keys$bedroom[row], states[state], sep = "_")
      details <- extract_model_details(regressions[[key]], pre_threshold, alpha)
      summary_matrix[row, state] <- glue::glue("N = {details$n_obs}:nSig = {details$n_sig}")

    }
  }

  summary_df <- data.frame(
    Dwelling = row_keys$dwelling,
    Bedrooms = row_keys$bedroom,
    summary_matrix,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

}

# HELPER ------------------------------

#' extract_model_details
#'
#' Pulls the coefficient table from a fitted `fixest` model, identifies the
#' pre-treatment ("lag") relative-time coefficients — those at or before
#' `pre_threshold` periods relative to treatment — and counts how many of
#' them are statistically significant at the given `alpha` level.
#'
#' @param model A fitted `fixest` model object (e.g. output of
#'   `fixest::feols()`) whose coefficient names encode a relative-time
#'   interaction term of the form `"...::<integer>:..."`.
#' @param pre_threshold Numeric. The relative-time cutoff (inclusive) below
#'   which a coefficient is treated as a pre-treatment "lag" coefficient.
#'   Coefficients with `relative_time <= pre_threshold` are checked for
#'   significance.
#' @param alpha Numeric. The significance level (e.g. `0.05`) below which a
#'   lag coefficient's p-value counts as significant.
#'
#' @returns A list with two elements:
#'   \describe{
#'     \item{n_obs}{Number of observations used to fit `model`.}
#'     \item{n_sig}{Number of pre-treatment lag coefficients with
#'       `Pr(>|t|)` below `alpha`.}
#'   }
extract_model_details <- function(model, pre_threshold, alpha) {

  coef_table <- fixest::coeftable(model) |>
    as.data.frame() |>
    tibble::rownames_to_column("term") |>
    dplyr::mutate(relative_time = as.numeric(stringr::str_match(term, "::(-?[0-9]+):")[, 2]))

  # pull the relative-time value out of coefficient names like
  # "Relative_Time_Binned::-3:Treated_GroupTRUE" -> -3

  lag_coefs <- coef_table |>
    dplyr::filter(!is.na(relative_time) & relative_time <= pre_threshold)

  num_sig_lag_coefs <- sum(lag_coefs$`Pr(>|t|)` < alpha)
  n_obs <- model[["nobs"]]

  stats <- list(n_obs = n_obs, n_sig = num_sig_lag_coefs)
}
