#' map_QLD_LGA_to_region
#'
#' Mutates the existing `Region` column to a data frame by classifying each Queensland
#' local government area (LGA) into its corresponding ABS Statistical Area
#' Level 4 (SA4) region (e.g. `Cairns`, `Wide Bay`, `Queensland - Outback`).
#' LGAs not matched by any of the listed groupings are left as `NA` in
#' `Region` — this includes `Brisbane` and `Moreton Bay`, both of which are
#' split across multiple SA4s and cannot be resolved to a single region from
#' the LGA name alone.
#'
#' @param data A data frame containing a `LGA` column with Queensland local
#'   government area names (e.g. `"Ipswich"`, `"Bundaberg"`).
#'
#' @return The input data frame with an added `Region` column giving each
#'   row's ABS SA4 region.
#' @export
map_QLD_LGA_to_region <- function(data){

  data <- data |>
    dplyr::mutate(Region = dplyr::case_when(
      # Cairns
      LGA %in% c("Cairns", "Cassowary Coast", "Douglas", "Tablelands", "Mareeba") ~ "Cairns",

      # Central Queensland
      LGA %in% c("Banana", "Central Highlands", "Gladstone", "Livingstone",
                 "Rockhampton") ~ "Central Queensland",

      # Darling Downs - Maranoa
      LGA %in% c("Goondiwindi", "Maranoa", "Southern Downs", "Western Downs") ~ "Darling Downs - Maranoa",

      # Gold Coast
      LGA == "Gold Coast" ~ "Gold Coast",

      # Ipswich
      LGA %in% c("Ipswich", "Somerset") ~ "Ipswich",

      # Logan - Beaudesert
      LGA %in% c("Logan", "Scenic Rim") ~ "Logan - Beaudesert",

      # Mackay - Isaac - Whitsunday
      LGA %in% c("Mackay", "Isaac", "Whitsunday") ~ "Mackay - Isaac - Whitsunday",

      # Queensland - Outback
      LGA %in% c("Cloncurry", "Cook", "Longreach", "Mount Isa", "Weipa") ~ "Queensland - Outback",

      # Sunshine Coast
      LGA %in% c("Sunshine Coast", "Noosa") ~ "Sunshine Coast",

      # Toowoomba
      LGA %in% c("Toowoomba", "Lockyer Valley") ~ "Toowoomba",

      # Townsville
      LGA %in% c("Townsville", "Burdekin", "Charters Towers", "Hinchinbrook") ~ "Townsville",

      # Wide Bay
      LGA %in% c("Bundaberg", "Fraser Coast", "Gympie", "North Burnett",
                 "South Burnett") ~ "Wide Bay",

      # Brisbane - East (Redland is wholly here; "Brisbane" LGA itself is not — see note)
      LGA == "Redland" ~ "Brisbane - East",

      LGA == "Brisbane" ~ "Brisbane",

      LGA == "Moreton Bay" ~ "Moreton Bay",

      TRUE ~ Region
    ))

}
