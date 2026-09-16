#' clean_sa_datasets
#'
#' This cleans all the datasets retrieved from download_sa_datasets and combines it into one dataframe.
#'
#' @param folder_path A string. Path to where you are holding the raw data files.
#' @seealso [download_sa_datasets()]
#'
#' @returns A dataframe. Final cleaned dataframe containing cleaned data from all data provided in the folder path.
#' @export
clean_sa_datasets <- function(folder_path) {
  file_names <- list.files(folder_path)

  cleaned_sheets <- lapply(file_names, function(file_name) {
    file_path <- paste0(folder_path, "/", file_name)
    sheets <- readxl::excel_sheets(file_path)
    required_sheet <- sheets[grepl("SLA", sheets)]
    raw_sheet <- readxl::read_xlsx(
      file_path,
      col_names = FALSE,
      sheet = required_sheet,
      .name_repair = "minimal"
    )

    cleaned_sheet <- clean_sa_sheet(raw_sheet, file_name)

    return(cleaned_sheet)
  })

  cleaned_data <- dplyr::bind_rows(cleaned_sheets)

  return(cleaned_data)

}

# HELPERS

#' clean_sa_sheet
#'
#' This cleans each individual SA quarterly rent excel LGA sheet.
#'
#' @param raw_sheet A dataframe. The raw sheet to clean
#' @param file_name The name of the file. Used to retrieve the month and year
#'
#' @returns A dataframe holding the cleaned LGA sheet
clean_sa_sheet <- function(raw_sheet, file_name) {
  words <- strsplit(file_name, " ")[[1]]
  month <- words[2]
  year <- words[3]

  quarter_map <- c(
    mar = "1",
    jun = "2",
    sep = "3",
    dec = "4"
  )

  quarter <- quarter_map[[month]]

  is_post_2020_Q3 <- year > 2020 || (year == 2020 && quarter > 3)

  if (is_post_2020_Q3) {
    cleaned_sheet <- clean_post_2020_Q3_sa_sheet(raw_sheet, file_name)
  } else {
    cleaned_sheet <- clean_pre_2020_Q3_sa_sheet(raw_sheet, file_name)
  }

  return(cleaned_sheet)

}



#' clean_pre_2020_Q3_sa_sheet
#'
#' This cleans each individual SA quarterly rent excel LGA sheet before 2020 Quarter 3 (inclusive).
#'
#' @param raw_sheet A dataframe. The raw sheet to clean
#' @param file_name The name of the file. Used to retrieve the month and year
#'
#' @returns A dataframe holding the cleaned LGA sheet
clean_pre_2020_Q3_sa_sheet <- function(raw_sheet, file_name) {
  row_to_load_past <- min(which(apply(raw_sheet, 1, function(x)
    any(
      grepl("Local Government Area", x)
    ))))
  raw_data <- raw_sheet[-(1:(row_to_load_past)), ]
  headers <- unlist(raw_data[1, ])
  filled_headers <- na.locf(headers, na.rm = FALSE)
  raw_data[1, ] <- as.list(filled_headers)


  true_headers <- mapply(paste,
                         raw_data[1, ],
                         raw_data[2, ],
                         MoreArgs = list(sep = "-"),
                         USE.NAMES = FALSE)[-(1:2)]
  true_headers <- c(as.character(raw_data[1, 1:2]), true_headers)
  raw_data <- raw_data[-(1:2), ]
  colnames(raw_data) <- trimws(gsub("\\s*\\([^)]*\\)", "", true_headers))

  required_columns <- raw_data |>
    tidyr::fill(`Local Government Area`, .direction = "down") |>
    tidyr::fill(`Statistical Local Area`, .direction = "down") |> # This gives the very last row which contains all of SA stastics a Total so we can keep it for.
    dplyr::filter(`Statistical Local Area` == "Total") |>
    dplyr::select(-`Statistical Local Area`) |>
    dplyr::rename(LGA = "Local Government Area")

  cols_to_pivot <- names(required_columns)[!names(required_columns)  %in% "LGA"]

  pivoted_dataframe <- required_columns |>
    tidyr::pivot_longer(all_of(cols_to_pivot),
                        names_to = "Measure",
                        values_to = "Values") |>
    mutate(
      Values = recode(Values, "*" = "5", "n.a." = NA_character_),
      LGA = trimws(gsub("\\s*\\([^)]*\\)", "", LGA)),
      Measure = gsub("BR", "", Measure),
      Measure = trimws(gsub("\\s+", " ", Measure))
    ) |>
    filter(!grepl("unknown", Measure, ignore.case = TRUE))

  # * only shows up for counts. Where there are 1 to 5 dwellings, the number is replaced with "*"

  required_columns <- pivoted_dataframe |>
    tidyr::separate_wider_delim(Measure,
                                delim = "-",
                                names = c("Dwelling", "Measure")) |>
    tidyr::separate_wider_delim(
      Dwelling,
      delim = " ",
      names = c("Bedrooms", "Dwelling"),
      too_few = "align_start"
    ) |>
    dplyr::mutate(
      Dwelling = recode(
        Dwelling,
        "Houses" = "House",
        "Flats" = "Flat",
        .missing = "All Properties"
      ),
      Bedrooms = recode(Bedrooms, "4+" = "4", "Total" = "All Sizes"),
      Measure = recode(Measure, "Median" = "Median_Rent", "Count" = "New_Bonds"),
      Region = ifelse(LGA == "South Australia", LGA, NA),
      LGA = ifelse(LGA == "South Australia", "Table Total", LGA)
    ) |>
    tidyr::pivot_wider(names_from = "Measure", values_from = "Values") |>
    dplyr::mutate(
      Median_Rent = as.numeric(Median_Rent),
      New_Bonds = as.numeric(New_Bonds)
    )

  date_part <- stringr::str_extract(file_name, "(?i)(mar|jun|sep|dec) \\d{4}")
  month <- stringr::str_extract(date_part, "[A-Za-z]+")
  year <- as.numeric(stringr::str_extract(date_part, "\\d{4}"))

  quarter <- dplyr::case_when(month == "mar" ~ 1,
                              month == "jun" ~ 2,
                              month == "sep" ~ 3,
                              month == "dec" ~ 4)

  sheet_with_additional_details <- required_columns |>
    dplyr::mutate(
      State = "SA",
      Year = year,
      Quarter = quarter,
      Quarter_Start_Date = dplyr::case_when(
        month == "mar" ~ as.Date(paste0(year, "-01-01")),
        month == "jun" ~ as.Date(paste0(year, "-04-01")),
        month == "sep" ~ as.Date(paste0(year, "-07-01")),
        month == "dec" ~ as.Date(paste0(year, "-10-01"))
      ),
      Quarter_End_Date = dplyr::case_when(
        month == "mar" ~ as.Date(paste0(year, "-03-31")),
        month == "jun" ~ as.Date(paste0(year, "-06-30")),
        month == "sep" ~ as.Date(paste0(year, "-09-30")),
        month == "dec" ~ as.Date(paste0(year, "-12-31"))
      )
    )

  return(sheet_with_additional_details)
}

#' clean_post_2020_Q3_sa_sheet
#'
#' This cleans each individual SA quarterly rent excel LGA sheet post 2020 Quarter 3 (exclusive).
#'
#' @param raw_sheet A dataframe. The raw sheet to clean
#' @param file_name The name of the file. Used to retrieve the month and year
#'
#' @returns A dataframe holding the cleaned LGA sheet
clean_post_2020_Q3_sa_sheet <- function(raw_sheet, file_name) {
  row_to_load_past <- min(which(apply(raw_sheet, 1, function(x)
    any(
      grepl("Local Government Area", x)
    ))))
  raw_data <- raw_sheet[-(1:(row_to_load_past)), ]

  is_first_row_column_label_text <- any(grepl("Column Labels", unlist(raw_data[1, ])))

  if (is_first_row_column_label_text){
    raw_data <- raw_data[-1,]
  }

  headers <- unlist(raw_data[1, ])
  filled_headers <- na.locf(headers, na.rm = FALSE)
  raw_data[1, ] <- as.list(filled_headers)
  is_count  <- grepl("count", filled_headers, ignore.case = TRUE)
  is_median <- grepl("median", filled_headers, ignore.case = TRUE)
  target_col <- is_count | is_median

  raw_data[2, target_col] <- "All Sizes"
  filled_bedroom_sizes <- na.locf(unlist(raw_data[2, ]), na.rm = FALSE)
  raw_data[2, ] <- as.list(filled_bedroom_sizes)

  is_na <- is.na(raw_data[3, ])
  raw_data[3, ] <- ifelse(is_count & is_na, "Count", raw_data[3, ])
  raw_data[3, ] <- ifelse(is_median & is_na, "Median", raw_data[3, ])

  raw_data[1, ] <- as.list(trimws(gsub(
    "Count|Median", "", raw_data[1, ], ignore.case = TRUE
  )))

  true_headers <- mapply(
    paste,
    raw_data[1, ],
    raw_data[2, ],
    raw_data[3, ],
    MoreArgs = list(sep = "-"),
    USE.NAMES = FALSE
  )[-1]
  true_headers <- c("LGA", true_headers)
  raw_data <- raw_data[-(1:3), ]
  colnames(raw_data) <- true_headers

  required_LGAs <- raw_data |>
    dplyr::mutate(LGA = trimws(gsub("\\s*\\([^)]*\\)", "", LGA))) |>
    dplyr::filter(grepl("Total", LGA)) |>
    dplyr::mutate(
      LGA = recode(LGA, "Grand Total" = "South Australia"),
      LGA = trimws(gsub("Total", "", LGA)),
      Region = ifelse(LGA == "South Australia", LGA, NA),
      LGA = ifelse(LGA == "South Australia", "Table Total", LGA)
    )

  cols_to_pivot <- names(required_LGAs)[!names(required_LGAs) %in% c("LGA", "Region")]

  pivoted_df <- required_LGAs |>
    tidyr::pivot_longer(cols_to_pivot, values_to = "Values", names_to = "Measure") |>
    dplyr::filter(!grepl("Unknown", Measure, ignore.case = TRUE)) |>
    tidyr::separate_wider_delim(Measure,
                                delim = "-",
                                names = c("Dwelling", "Bedrooms", "Measure")) |>
    dplyr::mutate(
      Bedrooms = trimws(gsub("Bedrooms|Bedroom", "", Bedrooms)),
      Dwelling = recode(
        Dwelling,
        "Flats/Units" = "Flat",
        "Houses" = "House",
        "Total" = "All Properties"
      ),
      Bedrooms = recode(Bedrooms, "4+" = "4"),
      Values = recode(Values, "*" = "5"),
      Measure = recode(Measure, "Median" = "Median_Rent", "Count" = "New_Bonds")
    ) |>
    tidyr::pivot_wider(names_from = "Measure", values_from = "Values") |>
    dplyr::mutate(
      Median_Rent = as.numeric(Median_Rent),
      New_Bonds = as.numeric(New_Bonds)
    )

  date_part <- stringr::str_extract(file_name, "(?i)(mar|jun|sep|dec) \\d{4}")
  month <- stringr::str_extract(date_part, "[A-Za-z]+")
  year <- as.numeric(stringr::str_extract(date_part, "\\d{4}"))

  quarter <- dplyr::case_when(month == "mar" ~ 1,
                              month == "jun" ~ 2,
                              month == "sep" ~ 3,
                              month == "dec" ~ 4)

  sheet_with_additional_details <- pivoted_df |>
    dplyr::mutate(
      State = "SA",
      Year = year,
      Quarter = quarter,
      Quarter_Start_Date = dplyr::case_when(
        month == "mar" ~ as.Date(paste0(year, "-01-01")),
        month == "jun" ~ as.Date(paste0(year, "-04-01")),
        month == "sep" ~ as.Date(paste0(year, "-07-01")),
        month == "dec" ~ as.Date(paste0(year, "-10-01"))
      ),
      Quarter_End_Date = dplyr::case_when(
        month == "mar" ~ as.Date(paste0(year, "-03-31")),
        month == "jun" ~ as.Date(paste0(year, "-06-30")),
        month == "sep" ~ as.Date(paste0(year, "-09-30")),
        month == "dec" ~ as.Date(paste0(year, "-12-31"))
      )
    )

  return(sheet_with_additional_details)
}
