#' download_nsw_datasets
#'
#' This function takes data from the NSW gov website and downloads all rent tables from 2018 onwards
#'
#' @param file_path Location to save files
#'
#' @returns Excel files saved in file path
#' @export
download_nsw_datasets <- function(file_path) {
  url <- "https://dcj.nsw.gov.au/about-us/families-and-communities-statistics/housing-rent-and-sales/previous-rent-and-sales-reports.html"
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

  rent_links <- links[grepl("rent tables", clean_links)]

  years <- rent_links |>
    sub(".*(\\d{4}).*", "\\1", x = _) |>
    as.numeric()

  dataset_links <- rent_links[years >= 2018]

  for (link in dataset_links) {
    file_name <- create_file_name(link)
    file_save <- paste0(file_path, "/", file_name)
    cleaned_link <- paste0("https://dcj.nsw.gov.au", link)

    download.file(url = cleaned_link,
                  destfile = file_save,
                  mode = "wb")

  }

}

# HELPERS

#' create_file_name
#'
#' @param link url to convert to a valid file name
#'
#' @returns A valid file name in the form "NSW month year quarter"
create_file_name <- function(link) {
  months <- c(
    january = "jan",
    february = "feb",
    march = "mar",
    april = "apr",
    may = "may",
    june = "jun",
    july = "jul",
    august = "aug",
    september = "sep",
    october = "oct",
    november = "nov",
    december = "dec"
  )

  month_pattern <- paste(months, collapse = "|") # for regex

  cleaned_file_name <- link |>
    sub(".*/", "", x = _) |>
    gsub("-", " ", x = _) |>
    gsub("_", " ", x = _) |>
    tolower() |>
    sub("\\.xlsx$", "", x = _) |>
    stringr::str_replace_all(months) |>
    sub(paste0(".*?(", month_pattern, ")"), "\\1", x = _)

  if (!grepl("quarter$", cleaned_file_name)) {
    cleaned_file_name <- paste(cleaned_file_name, "quarter")
  }

  cleaned_file_name <- paste0("NSW ", cleaned_file_name, ".xlsx")

  return(cleaned_file_name)

}
