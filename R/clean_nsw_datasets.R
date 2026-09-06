#' clean_nsw_datasets
#'
#' This cleans all the datasets retrieved from download_nsw_datasets and combines it into one dataframe.
#'
#' @param folder_path A string. Path to where you are holding the raw data files.
#' @seealso [download_nsw_datasets()]
#'
#' @returns A dataframe. Final cleaned dataframe containing cleaned data from all data provided in the folder path.
#' @export
clean_nsw_datasets <- function(folder_path) {
  file_names <- list.files(folder_path)
  cleaned_sheets <- lapply(file_names, function(file_name) {
    file_path <- paste0(folder_path, "/", file_name)
    raw_sheet <- readxl::read_xlsx(
      file_path,
      col_names = FALSE,
      sheet = "LGA",
      .name_repair = "minimal"
    )
    cleaned_sheet <- clean_nsw_sheet(raw_sheet, file_name)
    return(cleaned_sheet)
  })

  cleaned_dataset <- dplyr::bind_rows(cleaned_sheets)

  return(cleaned_dataset)

}

# HELPERS

#' clean_nsw_sheet
#'
#' This cleans each individual NSW quarterly rent excel LGA sheet.
#'
#' @param raw_sheet A dataframe. The raw sheet to clean
#' @param file_name The name of the file. Used to retrieve the month and year
#'
#' @returns A dataframe holding the cleaned LGA sheet
clean_nsw_sheet <- function(raw_sheet, file_name) {
  # Need to find where the actual table starts and trim the above rows

  row_to_load_from <- min(which(apply(raw_sheet, 1, function(x)
    any(grepl(
      "GMR", x
    ))))) - 1
  raw_sheet <- raw_sheet[-(1:row_to_load_from), ]

  # Clean column names

  names(raw_sheet) <- as.character(raw_sheet[1, ])
  raw_sheet <- raw_sheet[-1, ]
  names(raw_sheet) <- gsub("[\r\n$]", "", names(raw_sheet))

  # Select required columns and match to the VIC dataset names
  required_cols <- c(
    "Greater Metropolitan Region (GMR)",
    "GMR (Greater Metropolitan Region)",
    "Greater Sydney",
    "Rings",
    "Local Government Area (LGA)",
    "LGA (Local Government Areas)",
    "Dwelling Types",
    "Number of Bedrooms",
    "Bedroom Numbers",
    "Median Weekly Rent for New Bonds",
    "New Bonds LodgedNo."
  )
  sheet_with_required_cols <- raw_sheet %>%
    dplyr::select(any_of(required_cols)) %>%
    dplyr::rename(
      "Region" = "Rings",
      "LGA" = any_of(
        c("Local Government Area (LGA)", "LGA (Local Government Areas)")
      ),
      "GMR" = any_of(
        c("Greater Metropolitan Region (GMR)", "GMR (Greater Metropolitan Region)")
      ),
      "Dwelling" = "Dwelling Types",
      "Bedrooms" = any_of(c("Number of Bedrooms", "Bedroom Numbers")),
      "Median_Rent" = "Median Weekly Rent for New Bonds",
      "New_Bonds" = "New Bonds LodgedNo."
    ) %>%
    dplyr::filter(
      GMR == "Total",
      `Greater Sydney` == "Total"
    ) %>%
    dplyr::select(-c(GMR, `Greater Sydney`))

  # Filter sheets to only include data present in the VIC data-set
  # Also rename for consistent conventions
  filtered_sheet <- sheet_with_required_cols %>%
    dplyr::filter(
      Dwelling %in% c("House", "Total", "Flat/Unit"),
      Bedrooms %in% c(
        "Total",
        "1 Bedroom",
        "2 Bedrooms",
        "3 Bedrooms",
        "4 or more Bedrooms"
      )
    ) %>%
    mutate(
      Bedrooms = recode(
        Bedrooms,
        "Total" = "All Sizes",
        "1 Bedroom" = "1",
        "2 Bedrooms" = "2",
        "3 Bedrooms" = "3",
        "4 or more Bedrooms" = "4"
      ),
      Dwelling = recode(Dwelling, "Total" = "All Properties", "Flat/Unit" = "Flat"),
      Region = recode(Region, "Total" = "New South Wales"),
      LGA = case_when(
        Region == "New South Wales" & LGA == "Total" ~ "Table Total",
        Region != "New South Wales" &
          LGA == "Total" ~ "Group Total",
        TRUE ~ LGA
      )
    ) %>%
    dplyr::filter(Region != "New South Wales" |
                    LGA == "Table Total") %>%
    dplyr::distinct()

  value_corrected_sheet <- filtered_sheet %>%
    dplyr::mutate(
      Median_Rent = gsub(",", "", Median_Rent),
      New_Bonds = gsub(",", "", New_Bonds),
      across(
        c(Median_Rent, New_Bonds),
        ~ dplyr::case_when(
          .x == "s" ~ "30",
          .x == "-" ~ "10",
          TRUE ~ .x
        )
      ),
      New_Bonds = as.numeric(New_Bonds),
      Median_Rent = as.numeric(Median_Rent)
    )
  # ASSUMPTION: When a "-" is reported this means 10 or less bonds lodged. "s" is 30 or less bonds lodged.
  # Just assume it is the average.
  # ASSUMPTION: 4 or  more is just 4. Likely overstates the rent.
  # NOTE: If the Region is NSW and the LGA is Total, then Total is just the Table Total.
  # NOTE: If the Region is not NSW and the LGA is Total, then Total is just the Group Total.

  # For some reason the data-set duplicates its values. For example, the values for Region is Total
  # and LGA is Randwick and when Region is Inner Ring and LGA is Randwick are identical. This
  # applies for all LGAs. Therefore, just remove rows where Region is Total and LGA is not Table Total
  # as it is redundant information and we want to keep the Region field.

  # Need to find relavent year and quarter
  date_part <- stringr::str_extract(file_name, "(?i)(mar|jun|sep|dec) \\d{4}")
  month <- stringr::str_extract(date_part, "[A-Za-z]+")
  year <- as.numeric(stringr::str_extract(date_part, "\\d{4}"))

  quarter <- dplyr::case_when(month == "mar" ~ 1,
                              month == "jun" ~ 2,
                              month == "sep" ~ 3,
                              month == "dec" ~ 4)

  sheet_with_additional_details <- value_corrected_sheet %>%
    dplyr::mutate(
      State = "NSW",
      Year = year,
      Quarter = quarter,
      Quarter_Start_Date = case_when(
        month == "mar" ~ as.Date(paste0(year, "-01-01")),
        month == "jun" ~ as.Date(paste0(year, "-04-01")),
        month == "sep" ~ as.Date(paste0(year, "-07-01")),
        month == "dec" ~ as.Date(paste0(year, "-10-01"))
      ),
      Quarter_End_Date = case_when(
        month == "mar" ~ as.Date(paste0(year, "-03-31")),
        month == "jun" ~ as.Date(paste0(year, "-06-30")),
        month == "sep" ~ as.Date(paste0(year, "-09-30")),
        month == "dec" ~ as.Date(paste0(year, "-12-31"))
      )
    )

  return(sheet_with_additional_details)


}
