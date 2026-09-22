#' test_parallel_trends
#'
#' Fits an event-study difference-in-differences specification for each
#' combination of dwelling type, bedroom count, and candidate control
#' state, comparing the treated state against each control around the
#' specified treatment date. See report for more details.
#'
#' @param data A data frame containing the panel data, including (at
#'   least) the columns `State`, `Dwelling`, `Bedrooms`,
#'   `Year`, `Quarter`, and `New_Bonds`.
#' @param treated A character string giving the name of the treated state
#'   (e.g. `"VIC"`).
#' @param controls A character vector of candidate control states. A
#'   separate event-study regression is estimated against `treated`
#'   for each control state, within each dwelling type and bedroom count
#'   combination.
#' @param treatment_time A length-2 numeric vector giving the treatment
#'   date as `c(year, quarter)`. Defaults to `c(2021, 1)`.
#' @param bin_width Integer giving the number of quarters to keep as
#'   individual leads/lags around treatment before pooling the rest into
#'   endpoint bins. `Relative_Time` is capped at `+bin_width` and
#'   `-bin_width`, where the end points represent bin_width or more. Defaults to `4`.
#' @param baseline_quarter The relative-time value used as the omitted reference
#'   period in the event-study specification (passed to `ref` in
#'   `fixest::i()`). Defaults to `-1`, the quarter immediately preceding
#'   treatment.
#' @param unit A character string giving the unit of observation,
#'   either `"State"` or `"LGA"`. Determines the fixed effects
#'   (`State + Quarter` vs. `LGA + Quarter`), the clustering/vcov used
#'   (heteroskedasticity-robust for `"State"`, clustered by `Region` for
#'   `"LGA"`), and which columns are treated as controls.
#'
#' @return A named list of `fixest` model objects, one per
#'   dwelling/bedroom/control combination. Each element is the fitted
#'   event-study regression for that segment, containing the estimated
#'   relative-time coefficients (leads and lags around `treatment_time`)
#'   used to assess parallel trends. List names take the form
#'   `"<dwelling>_<bedroom>_<control>"`.
#'
#' @export
test_parallel_trends <- function(data,
                                 treated,
                                 controls,
                                 treatment_time = c(2021, 1),
                                 bin_width = 4,
                                 baseline_quarter = -1,
                                 unit) {

  subset_details <- data |>
    dplyr::filter(State != treated) |>
    dplyr::rename(dwelling = "Dwelling", bedroom = "Bedrooms") |>
    dplyr::distinct(dwelling, bedroom) |>
    tidyr::crossing(control = controls)

  # Four changes need to be made.
  # First: Have a Quarter column that expresses the Years in Quarter
  # Second: Have a relative time Quarter to the specified treatment date
  # Third: Create a treatment group dummy
  # Fourth: Remove unnecessary columns if they exist

  regression_data <- data |>
    dplyr::mutate(Quarter = (Year - min(Year)) * 4 + Quarter,
                  Treatment_Time = (treatment_time[1] - min(Year)) * 4 + treatment_time[2],
                  Relative_Time = Quarter - Treatment_Time,
                  Relative_Time_Binned = pmax(pmin(Relative_Time, bin_width), -bin_width),
                  Treated_Group = ifelse(State == "VIC", TRUE, FALSE)) |>
    dplyr::select(-dplyr::any_of(c("New_Bonds",
                                   "Year",
                                   "Quarter_Start_Date",
                                   "Quarter_End_Date",
                                   "Treatment_Time",
                                   "Relative_Time")))

  regressions <- purrr::pmap(subset_details, function(dwelling, bedroom, control) {

    segment_data <- regression_data |>
      dplyr::filter(Dwelling == dwelling,
                    State %in% c(treated, control),
                    Bedrooms == bedroom) |>
      dplyr::select(-c("Dwelling", "Bedrooms"))

    if (unit == "State"){
      non_controls <- c("Median_Rent", "Relative_Time_Binned", "Treated_Group", "State", "Quarter")
      control_vars <- setdiff(names(segment_data), non_controls)
      control_vars <- paste0("`", control_vars, "`")

      regression <- fixest::feols(
        fixest::xpd(Median_Rent ~ fixest::i(Relative_Time_Binned, Treated_Group, ref = baseline_quarter) + ..ctrl | State + Quarter,
                    ..ctrl = control_vars),
        data = segment_data,
        vcov = "hetero"
      )
    } else if (unit == "LGA") {

      non_controls <- c("Median_Rent", "Relative_Time_Binned", "Treated_Group", "LGA", "Quarter", "State", "Region")
      control_vars <- setdiff(names(segment_data), non_controls)
      control_vars <- paste0("`", control_vars, "`")

      regression <- fixest::feols(
        fixest::xpd(Median_Rent ~ fixest::i(Relative_Time_Binned, Treated_Group, ref = baseline_quarter) + ..ctrl | LGA + Quarter,
                    ..ctrl = control_vars),
        data = segment_data,
        cluster = ~Region
      )
    }


  })

  names(regressions) <- paste(subset_details$dwelling, subset_details$bedroom, subset_details$control, sep = "_")

  return(regressions)

}
