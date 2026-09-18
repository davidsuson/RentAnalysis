#' join_controls
#'
#' @param data combined_dataset_no_controls.xlsx
#' @param controls cleaned income and demographic data
#' @seealso [clean_personal_income_statistics()], [clean_demographic_statistics()]
#'
#' @returns data with controls join by State, LGA and Year.
#' @export
join_controls <- function(data, controls){

  missing_names_list <- lapply(controls, function(control) {
    check_missing_LGA_names(data, control)
  })

  missing_names_list <- Filter(Negate(is.null), missing_names_list)

  if (length(missing_names_list) > 0) {
    stop(
      "The following LGA names were not found in the control datasets and need to be manually mapped:\n",
      paste(unique(unlist(missing_names_list)), collapse = ", "),
      call. = FALSE
    )
  }

  cols_to_left_join <- intersect(colnames(data), c("Year", "State", "LGA"))

  final_data_with_controls <- purrr::reduce(controls, dplyr::left_join, by = cols_to_left_join, .init = data)

  return(final_data_with_controls)
}


# HELPER

#' check_missing_LGA_names
#'
#' @param data combined_dataset_no_controls.xlsx
#' @param control one of the control datasets
#'
#' @returns NULL or vector of names that are not directly named in the control dataset. These names need to be mapped manually.
check_missing_LGA_names <- function(data, control){

  missing_names <- setdiff(data$LGA, control$LGA)

  if (length(missing_names) == 0) {
    return(NULL)
  }

  return(missing_names)
}
