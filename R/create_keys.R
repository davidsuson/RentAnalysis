#' create_keys
#'
#' creates a mapping key for income earners,by geography Table 1.5 LGA names to LGA names in combined_dataset_no_controls.xlsx.
#' This only maps names that are not identical. The key will be used to replace the values before a left join occurs.
#'
#' @returns A dataframe
#' @export
create_keys <- function() {

  missing_rent_data_income_names <- c(
    "Ku-Ring-Gai",
    "Gundagai",
    "Western Plains Regional",
    "Sydney City",
    "Sutherland",
    "The Hills",
    "Bathurst",
    "Glen Innes",
    "Greater Hume",
    "Tamworth",
    "Upper Hunter",
    "Upper Lachlan",
    "Warrumbungle",
    "Mornington Penin'a",
    "Mallala"
  )

  matching_LGA_income_names <- c(
    "Ku-ring-gai",
    "Cootamundra-Gundagai Regional",
    "Dubbo Regional",
    "Sydney",
    "Sutherland Shire",
    "The Hills Shire",
    "Bathurst Regional",
    "Glen Innes Severn",
    "Greater Hume Shire",
    "Tamworth Regional",
    "Upper Hunter Shire",
    "Upper Lachlan Shire",
    "Warrumbungle Shire",
    "Mornington Peninsula",
    "Adelaide Plains"
  )

  lga_name_mapping <- tibble::tibble(
    missing_rent_data_income_names,
    matching_LGA_demo_names
  )

  return(lga_demo_key)
}
