#' clean_qld_dataset
#'
#' This function takes the raw qld dataset (that has had some manual modifications made to it) and appends
#' all the sheets together into the required shape
#'
#' @param raw_sheet_path File path to raw qld data file
#'
#' @returns A dataframe with all the data from the excel sheet
#' @export
clean_qld_dataset <- function(raw_sheet_path) {
  sheet_names <- c("7 lga-rents",
                   "8 lga-new-bonds",
                   "10 qld-rents",
                   "11 qld-new-bonds")

  cleaned_sheets <- lapply(sheet_names, function(sheet_name) {
    raw_sheet <- readxl::read_xlsx(
      raw_sheet_path,
      col_names = FALSE,
      sheet = sheet_name,
      .name_repair = "minimal"
    )
    cleaned_sheet <- clean_qld_sheet(raw_sheet, sheet_name)
    return(cleaned_sheet)
  })

  names(cleaned_sheets) <- sheet_names
  join_columns = c("Dwelling","Bedrooms","Quarter","Year", "LGA")

  combined_lga <- cleaned_sheets[["7 lga-rents"]] |>
    dplyr::left_join(dplyr::select(cleaned_sheets[["8 lga-new-bonds"]], all_of(c("New_Bonds",join_columns))),
                     by = join_columns)

  combined_qld <- cleaned_sheets[["10 qld-rents"]] |>
    dplyr::left_join(dplyr::select(cleaned_sheets[["11 qld-new-bonds"]], all_of(c("New_Bonds",join_columns))),
                     by = join_columns)

  cleaned_dataset <- dplyr::bind_rows(combined_lga, combined_qld)

  return(cleaned_dataset)

}

# HELPER

#' clean_qld_sheet
#'
#' This function takes either qld-rents or qld-new-bonds from the qld data file, changes its shape, cleans values and columns and adds
#' additional columns
#'
#' @param raw_sheet A dataframe passed from clean_vic_dataset
#' @param sheet_name A string passed from clean_vic_dataset
#'
#' @returns A dataframe containing the cleaned sheet
clean_qld_sheet <- function(raw_sheet, sheet_name) {
  if (grepl("rent", sheet_name)) {
    value_col_name = "Median_Rent"
  } else if (grepl("new-bonds", sheet_name)) {
    value_col_name = "New_Bonds"
  }

  raw_sheet <- raw_sheet[-c(1:4), -1]
  cleaned_dates <- mapply(paste, raw_sheet [2, ], raw_sheet [3, ], USE.NAMES = FALSE)[-c(1:2)]
  cleaned_names <- c(as.character(raw_sheet [1, 1:2]), cleaned_dates)
  raw_sheet  <- raw_sheet[-c(1:3), ]
  colnames(raw_sheet) <- cleaned_names

  pivoted_data <- raw_sheet |>
    tidyr::pivot_longer(
      cols = tidyr::all_of(cleaned_dates),
      names_to = "Date",
      values_to = value_col_name
    )

  if ("Local Government Area" %in% names(pivoted_data)) {

    cleaned_location <- pivoted_data |>
      dplyr::rename(LGA = "Local Government Area") |>
      dplyr::mutate(LGA = trimws(gsub("\\s*\\([^)]*\\)", "", LGA)))

  } else if ("State" %in% names(pivoted_data)) {

    cleaned_location <- pivoted_data |>
      dplyr::rename(Region = "State") |>
      dplyr::mutate(LGA = "Table Total")

  }

  cleaned_columns <- cleaned_location |>
    dplyr::mutate(!!value_col_name := as.numeric(!!rlang::sym(value_col_name))) |>
    dplyr::filter(grepl("Flat|House|All dwellings", Dwelling)) |>
    tidyr::separate_wider_delim(Dwelling,
                                delim = " ",
                                names = c("Dwelling", "Bedrooms")) |>
    tidyr::separate_wider_delim(Date, delim = " ", names = c("Quarter", "Year")) |>
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
      Dwelling = dplyr::replace_values(Dwelling, "All" ~ "All Properties"),
      Bedrooms = dplyr::replace_values(Bedrooms, "dwellings" ~ "All Sizes"),
      State = "QLD"
    ) |>
    dplyr::mutate(
      Quarter = dplyr::case_when(
        Quarter == "Mar" ~ 1,
        Quarter == "Jun" ~ 2,
        Quarter == "Sep" ~ 3,
        Quarter == "Dec" ~ 4
      ),
      Year = as.numeric(Year)
    )

  final_data <- cleaned_columns

  return(final_data)
}
