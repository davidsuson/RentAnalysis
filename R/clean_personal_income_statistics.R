LGA_data <- readxl::read_xlsx("C:/Users/David/Desktop/MicroEcon - Rent/RentAnalysis/Data/Raw Data/controls/income,earners by geography.xlsx",
                              sheet = "Table 1.5",
                              col_names = FALSE,
                              .name_repair = "minimal")

LGA_data <- LGA_data[-(1:5),]
empty_row_index <- which(rowSums(!is.na(LGA_data)) == 0)[1]
LGA_data <- LGA_data[seq_len(empty_row_index - 1), ]

headers <- unlist(LGA_data[1,])
cleaned_headers <- trimws(gsub("\\s*\\([^)]*\\)", "", headers))
filled_headers <- na.locf(cleaned_headers, na.rm = FALSE)
LGA_data[1,] <-as.list(filled_headers)

combined_headers <- mapply(
  paste,
  LGA_data[1,],
  LGA_data[2,],
  MoreArgs = list(sep = ":"),
  USE.NAMES = FALSE
)[-c(1:2)]

colnames(LGA_data) <- c(LGA_data[2, 1:2], combined_headers)
LGA_data <- LGA_data[-c(1:2),]

cleaned_LGA_data <- LGA_data |>
  dplyr::rename(State = LGA,
                LGA = "LGA NAME") |>
  dplyr::mutate(State = recode(State,
                               "New South Wales" = "NSW",
                               "Victoria" = "VIC",
                               "Queensland" = "QLD",
                               "South Australia" = "SA",
                               "Western Australia" = "WA",
                               "Tasmania" = "TAS",
                               "Northern Territory" = "NT",
                               "Australian Capital Territory" = "ACT"),
                State = sub("^[0-9]+$", NA_character_, State)) |>
  tidyr::fill(State, .direction = "down") |>
  dplyr::mutate(LGA = coalesce(LGA, State))


cols_to_pivot <- colnames(cleaned_LGA_data)[!colnames(cleaned_LGA_data) %in% c("State", "LGA")]

pivoted_LGA_data <- cleaned_LGA_data |>
  tidyr::pivot_longer(cols_to_pivot,
                      names_to = "Measure",
                      values_to = "Values") |>
  dplyr::mutate(Values = recode(Values, "np" = NA_character_),
                Values = as.numeric(Values)) |>
  tidyr::separate_wider_delim(Measure,
                              ":",
                              names = c("Measure","Year")) |>
  dplyr::filter(
    Measure %in% c("Earners", "Median age of earners", "Median")
  ) |>
  dplyr::mutate(
    Measure = recode(Measure, "Earners" = "Num_Earners", "Median age of earners" = "Median Age", "Median" = "Median Income"),
    Year = as.numeric(stringr::str_extract(Year, "^\\d{4}"))
  )

final_LGA_data <- pivoted_LGA_data  |>
  tidyr::pivot_wider(
    names_from = Measure,
    values_from = Values
  )
