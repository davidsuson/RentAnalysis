#' create_keys
#'
#' creates a mapping key for income earners,by geography Table 1.5 LGA names to LGA names in combined_dataset_no_controls.xlsx.
#' This only maps names that are not identical. The key will be used to replace the values before a left join occurs.
#'
#' @returns A list of data-frames mapping missing LGA names from the rent dataset to the LGA names in the control tables
#' @export
create_keys <- function() {

  income_LGA_name_key <- create_income_table_key()

  keys <- list(
    income = income_LGA_name_key,
    demo = NULL
  )

  return(keys)
}

# HELPERS

#' create_income_table_key
#'
#' Some LGA names in combined_dataset_no_controls.xlsx won't be present in income,earners by geography.xlsx.
#' This just due to different names for the same LGA. This function lists the names in combined_dataset_no_controls.xlsx,
#' that are not present in income,earners by geography.xlsx.
#'
#' It then lists, the equivalent names in income,earners by geography.xlsx in the vector matching_LGA_name_in_income_table
#'
#' @returns A tibble mapping the missing names in combined_dataset_no_controls.xlsx to their equivalent names in income,earners by geography.xlsx
create_income_table_key <- function(){

  rent_data_LGA_name_with_no_match <- c(
    "Ku-Ring-Gai",
    "Gundagai",
    "Western Plains Regional",
    "Adelaide Plains"
  )

  matching_LGA_name_in_income_table <- c(
    "Ku-ring-gai",
    "Cootamundra-Gundagai Regional",
    "Dubbo Regional",
    "Mallala"
  )

  income_LGA_name_key <- tibble::tibble(
    rent_data_LGA_name_with_no_match,
    matching_LGA_name_in_income_table
  )

  return(income_LGA_name_key)
}

#' create_demographic_table_key
#'
#' Some LGA names in combined_dataset_no_controls.xlsx won't be present in age and sex demographics.xlsx.
#' This just due to different names for the same LGA. This function lists the names in combined_dataset_no_controls.xlsx,
#' that are not present in age and sex demographics.xlsx.
#'
#' It then lists, the equivalent names in age and sex demographics.xlsx in the vector matching_LGA_name_in_demo_table
#'
#' @returns A tibble mapping the missing names in combined_dataset_no_controls.xlsx to their equivalent names in age and sex demographics.xlsx
create_demographic_table_key <- function(){

  rent_data_LGA_name_with_no_match <- c(
    "Ku-Ring-Gai",
  )

  matching_LGA_name_in_demo_table <- c(
    "Ku-ring-gai",
  )

  demo_LGA_name_key <- tibble::tibble(
    rent_data_LGA_name_with_no_match,
    matching_LGA_name_in_demo_table
  )

  return(demo_LGA_name_key)
}
