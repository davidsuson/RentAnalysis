#' run_did_regressions
#'
#' Runs Callaway and Sant'Anna (2021) difference-in-differences regressions
#' for one outcome across every state-pair file and dwelling/bedroom sheet,
#' for both the non-control and control datasets, and summarises each model
#' with [summarise_did()].
#'
#' @param non_control_folder A string. Path to the folder of Excel files for
#'   the non-control samples (one file per state pair).
#' @param control_folder A string. Path to the folder of Excel files for the
#'   control samples (one file per state pair).
#' @param outcome A string. Name of the outcome column (e.g. `"Median_Rent"`).
#'   Also used as a regular expression to select files in each folder.
#'
#' @returns A list with two elements, `non_control` and `control`. Each is a
#'   named list of state pairs (e.g. `VIC_NSW`), and each state pair is a named
#'   list of dwelling/bedroom sheets holding the output of [summarise_did()].
#'   For example, `result$control$VIC_QLD$Flat_1$att_se`.
#'
#' @seealso [run_did_on_folder()], [summarise_did()]
#' @export
run_did_regressions <- function(non_control_folder,
                                control_folder,
                                outcome) {

  non_control_atts <- run_did_on_folder(folder_path = non_control_folder,
                              outcome = outcome,
                              control = FALSE)

  control_atts <- run_did_on_folder(folder_path = control_folder,
                         outcome = outcome,
                         control = TRUE)

  list(
    non_control = purrr::map_depth(non_control_atts, 2, summarise_did),
    control     = purrr::map_depth(control_atts, 2, summarise_did)
  )

}

# HELPERS ----------------------------------------------------------------------

#' run_did_on_folder
#'
#' Runs [run_did()] on every Excel file in a folder whose name matches
#' `outcome`. Each file is labelled by its state pair, taken from the 4th and
#' 5th underscore-separated words of the file name.
#'
#' @param folder_path A string. Path to the folder of Excel files.
#' @param outcome A string. Name of the outcome column, also used as a regular
#'   expression to select files.
#' @param control Logical. Whether to include covariates (passed to
#'   [run_did()]).
#'
#' @returns A named list with one element per state pair, each a named list of
#'   `att_gt` objects (one per sheet).
#'
#' @seealso [run_did()]
#' @keywords internal
run_did_on_folder <- function(folder_path, outcome, control){

  file_paths <- list.files(folder_path, pattern = outcome, full.names = TRUE)
  state_pairs <- stringr::word(tools::file_path_sans_ext(basename(file_paths)), 4, 5, sep = "_")

  folder_dids <- lapply(file_paths, run_did, outcome, control)
  names(folder_dids) <- state_pairs

  return(folder_dids)

}

#' run_did
#'
#' Runs [did::att_gt()] on every sheet of an Excel workbook. Uses never-treated
#' units as the comparison group, a universal base period, and standard errors
#' clustered by region.
#'
run_did <- function(file_path, outcome, control) {

  if (control){
    control_formula <- stats::reformulate(c(
      "Num_Earners", "Median_Age", "Median_Income"
    ))

  } else {
    control_formula <- ~1
  }

  individual_att_data <- readxl::excel_sheets(file_path) |>
    purrr::set_names() |>
    purrr::map(function(sheet) {
      data <- readxl::read_excel(file_path, sheet = sheet)
      individual_att <- did::att_gt(
        yname = outcome,
        tname = "Running_Quarter",
        idname = "LGA_ID",
        gname = "Treated_Time",
        xformla = control_formula,
        data = data,
        control_group = "nevertreated",
        base_period = "universal",
        clustervars = "Region"
      )
    })

}


#' summarise_did
#'
#' Summarises an `att_gt` result: the parallel trends pre-test p-value, the
#' simple aggregate ATT and its standard error, and a plot of the
#' calendar-time effects.
#'
summarise_did <- function(att) {

  simple   <- did::aggte(att, type = "simple")
  calendar <- did::aggte(att, type = "calendar")
  dynamic  <- did::aggte(att, type = "dynamic")

  list(
    pretest_p     = if (is.null(att$Wpval)) NA_real_ else att$Wpval,
    att           = simple$overall.att,
    se            = simple$overall.se,
    att_se        = sprintf("%.2f (%.2f)", simple$overall.att, simple$overall.se),
    calendar_plot = did::ggdid(calendar),
    event_plot    = did::ggdid(dynamic),
    model         = att
  )
}
