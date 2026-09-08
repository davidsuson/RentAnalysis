#' clean_vic_dataset
#'
#' This function takes the raw vic dataset (that has had some manual modifications made to it) and appends
#' all the sheets together into the required shape
#'
#' @param raw_sheet_path File path to raw vic data file
#'
#' @returns A dataframe with all the data from the excel sheet
#' @export
clean_vic_dataset <- function(raw_sheet_path) {
  sheet_names <-  readxl::excel_sheets(raw_sheet_path)

  cleaned_sheets <- lapply(sheet_names, function(sheet_name) {
    raw_sheet <- readxl::read_xlsx(
      raw_sheet_path,
      col_names = FALSE,
      sheet = sheet_name,
      .name_repair = "minimal"
    )
    cleaned_sheet <- clean_vic_sheet(raw_sheet, sheet_name)
    return(cleaned_sheet)
  })

  cleaned_dataset <- dplyr::bind_rows(cleaned_sheets)

  return(cleaned_dataset)

}

# HELPER

#' clean_vic_sheet
#'
#' This function takes a single sheet from the vic data file, changes its shape, cleans values and columns and adds
#' additional columns
#'
#' @param raw_sheet A dataframe passed from clean_vic_dataset
#' @param sheet_name A string passed from clean_vic_dataset
#'
#' @returns A dataframe containing the cleaned sheet
clean_vic_sheet <- function(raw_sheet, sheet_name) {
  # removing redundant rows and making a single header

  raw_sheet <- raw_sheet[-1, ]
  raw_sheet[1, ][is.na(raw_sheet[1, ])] <- " "
  new_names <- paste0(raw_sheet[1, ], " ", raw_sheet[2, ])
  names(raw_sheet) <- new_names
  raw_sheet <- raw_sheet[-c(1, 2), ]

  # restructure data to required shape

  cols_to_pivot <- names(raw_sheet)[!names(raw_sheet) %in% c("  LGA", "  Region")]
  pivoted_data <- raw_sheet |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols_to_pivot),
      names_to = "Date",
      values_to = "Value"
    ) |>
    tidyr::separate(Date,
                    into = c("Quarter", "Year", "Measure"),
                    sep = " ")

  names(pivoted_data) <- names(pivoted_data) |>
    gsub("  ", "", x = _)

  # add required columns

  if (sheet_name != "All Properties") {
    num_br <- as.numeric(stringr::str_extract(sheet_name, "^\\d+"))
  } else {
    num_br <- "All Sizes"
  }

  dwelling <- stringr::str_trim(stringr::str_remove(sheet_name, "^\\d+br\\s*"))

  data_with_additional_columns <- pivoted_data |>
    dplyr::mutate(dplyr::across(Value, ~ na_if(.x, "-"))) |>
    tidyr::pivot_wider(names_from = Measure, values_from = Value) |>
    dplyr::mutate(
      Quarter_Start_Date = dplyr::case_when(
        Quarter == "Mar" ~ as.Date(paste0(Year, "-01-01")),
        Quarter == "Jun" ~ as.Date(paste0(Year, "-04-01")),
        Quarter == "Sep" ~ as.Date(paste0(Year, "-07-01")),
        Quarter == "Dec" ~ as.Date(paste0(Year, "-10-01"))
      ),
      Quarter_End_Date = dplyr::case_when(
        Quarter == "Mar" ~ as.Date(paste0(Year, "-03-31")),
        Quarter == "Jun" ~ as.Date(paste0(Year, "-06-30")),
        Quarter == "Sep" ~ as.Date(paste0(Year, "-09-30")),
        Quarter == "Dec" ~ as.Date(paste0(Year, "-12-31"))
      ),
      State = "VIC",
      Bedrooms = num_br,
      Dwelling = dwelling
    ) |>
    dplyr::mutate(Bedrooms = as.character(Bedrooms))
  # This is because All Properties will have All Size in the bedroom field

  # clean up columns

  data_with_cleaned_columns <- data_with_additional_columns |>
    dplyr::mutate(
      Quarter = dplyr::case_when(
        Quarter == "Mar" ~ 1,
        Quarter == "Jun" ~ 2,
        Quarter == "Sep" ~ 3,
        Quarter == "Dec" ~ 4
      )
    ) |>
    dplyr::rename('New_Bonds' = "Count", 'Median_Rent' = "Median") |>
    dplyr::mutate(
      New_Bonds = as.numeric(New_Bonds),
      Median_Rent = as.numeric(Median_Rent),
      Year = as.numeric(Year)
    )

  final_data <- data_with_cleaned_columns |>
    dplyr::filter(Year >= 2018)

  return(final_data)
}
