#' clean_demographic_statistics
#'
#' @param raw_sheet_path Path to age and sex demographics.xlsx.
#'
#' @returns A table with % of men, % of females, total males and total females by state and LGA by year
#' @export
clean_demographic_statistics <- function(raw_sheet_path){

  sheet_names <- c("Table 1", "Table 2")

  cleaned_sheets <- lapply(sheet_names, function(sheet_name) {
    raw_sheet <- readxl::read_xlsx(
      raw_sheet_path,
      col_names = FALSE,
      sheet = sheet_name,
      .name_repair = "minimal"
    )
    cleaned_sheet <- clean_demographic_table(raw_sheet)
    return(cleaned_sheet)
  })

  cleaned_dataset <- purrr::reduce(cleaned_sheets, dplyr::left_join, by = c("Year", "State", "LGA")) |>
    dplyr::mutate(Year = as.numeric(Year))

  return(cleaned_dataset)

}


# HELPERS

#' clean_demographic_table
#'
#' @param raw_data Data loaded from a table in age and sex demographics.xlsx.
#'
#' @returns Table which lists % of population as sex for age buckets and total population for each state and LGA by year
clean_demographic_table <- function(raw_data){

  raw_data <- raw_data[5:13706, ]
  age_groups <- unlist(raw_data[1, 6:ncol(raw_data)], use.names = FALSE)

  age_groups <- gsub("\\s*and\\s+over", "+", age_groups, ignore.case = TRUE)
  last_index <- length(age_groups)
  sex_label <- if (grepl("female", age_groups[last_index], ignore.case = TRUE)) "F" else "M"

  # Collapse the total label, then prefix everything
  age_groups[last_index] <- "Total"
  age_groups <- paste(sex_label, age_groups)
  colnames(raw_data) <- c(unlist(raw_data[2, ][1:5], use.names = FALSE), age_groups)
  cleaned_data <- raw_data[-c(1:2), ]

  LGA_demo <- cleaned_data |>
    dplyr::select(-c("S/T code", "LGA code")) |>
    dplyr::rename(State = "S/T name", LGA = "LGA name") |>
    dplyr::mutate(
      State = dplyr::recode_values(
        State,
        "New South Wales" ~ "NSW",
        "Victoria" ~ "VIC",
        "Queensland" ~ "QLD",
        "South Australia" ~ "SA",
        "Western Australia" ~ "WA",
        "Tasmania" ~ "TAS",
        "Northern Territory" ~ "NT",
        "Australian Capital Territory" ~ "ACT",
        "Australia" ~ "AUS"
      ),
      LGA = trimws(gsub("\\s*\\([^)]*\\)", "", LGA)),
      dplyr::across(dplyr::all_of(age_groups), as.numeric)
    ) |>
    dplyr::filter(State %in% c("VIC", "QLD", "NSW", "SA"),
                  Year >= 2018)

  state_demo <- LGA_demo |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(age_groups), \(x) sum(x, na.rm = TRUE)),
      .by = c(Year, State)
    )

  convert_to_percent <- list(
    LGA = LGA_demo,
    State = state_demo
  )

  cleaned_table <- lapply(convert_to_percent, convert_demographic_stats_to_percentage, age_groups) |>
    dplyr::bind_rows() |>
    dplyr::mutate(LGA = dplyr::coalesce(LGA, State))

  return(cleaned_table)

}

#' convert_demographic_stats_to_percentage
#'
#' @param data data to calculate % from
#' @param age_groups column names of age groups to calculate as a % of row total
#'
#' @returns Resident population table in % of row total
convert_demographic_stats_to_percentage <- function(data, age_groups){
  buckets <- age_groups[-length(age_groups)]
  total_col <- age_groups[length(age_groups)]
  percentage_data <- data |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(buckets),
      ~ .x / .data[[total_col]]
    ))
}
