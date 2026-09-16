#' download_sa_datasets
#'
#' This function takes data from the SA gov website and downloads all rent tables from 2018 onwards
#'
#' @param file_path Location to save files
#'
#' @returns Excel files saved in file path
#' @export
download_sa_datasets <- function(file_path) {
  url <- "https://data.sa.gov.au/data/dataset/private-rent-report"
  page <- rvest::read_html(url)

  # look for html elements which have a hyperlink
  links <- page %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href")

  # clean links so can look for links which says rent tables
  clean_links <- links |>
    sub(".*/", "", x = _) |>
    gsub("-", " ", x = _) |>
    gsub("_", " ", x = _) |>
    tolower()

  rent_links <- links[grepl(".xlsx", clean_links)]

  years <- rent_links |>
    sub(".*(\\d{4}).*", "\\1", x = _) |>
    as.numeric()

  dataset_links <- rent_links[years >= 2018]

  for (link in dataset_links) {
    file_name <- create_sa_file_name(link)
    file_save <- paste0(file_path, "/", file_name)

    download.file(url = link,
                  destfile = file_save,
                  mode = "wb")

  }

}

# HELPERS

#' create_sa_file_name
#'
#' @param link url to convert to a valid file name
#'
#' @returns A valid file name in the form "SA month year quarter"
create_sa_file_name <- function(link) {
  months <- c(
    mar = "03",
    jun = "06",
    sep = "09",
    dec = "12"
  )

  month_pattern <- paste(months, collapse = "|") # for regex

  cleaned_file_name <- link |>
    sub(".*/", "", x = _) |>
    gsub("-", " ", x = _) |>
    gsub("_", " ", x = _) |>
    tolower() |>
    sub("amended", "", x = _) |>
    sub("\\.xlsx$", "", x = _)

  words <- strsplit(cleaned_file_name, " ")[[1]]
  last_two <- tail(words, 2)
  flipped <- rev(last_two)
  month_name <- names(months)[months == flipped[1]]

  cleaned_file_name <- paste("SA", month_name, flipped[2], "quarter.xlsx")

  return(cleaned_file_name)

}
