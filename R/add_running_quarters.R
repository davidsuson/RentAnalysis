#' add_running_quarters
#'
#' Adds a `Running_Quarter` column that numbers quarters consecutively across
#' years, with 1 being the earliest Year/Quarter in `data`. Gaps are
#' preserved: if a quarter is missing from the data entirely, its number is
#' skipped.
#'
#' @param data A data frame with columns `Year` and `Quarter` (numeric,
#'   1--4).
#'
#' @returns `data` with an added integer column `Running_Quarter`.
#'
#' @keywords internal
add_running_quarters <- function(data){

  data |>
    dplyr::mutate(
      qtr_num = Year * 4 + Quarter,
      Running_Quarter = qtr_num - min(qtr_num) + 1
    ) |>
    dplyr::select(-qtr_num)

}
