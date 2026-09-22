#' map_SA_LGA_to_region
#'
#' Mutates an existing `Region` column to a data frame by classifying each South
#' Australian local government area (LGA) into its corresponding ABS
#' Statistical Area Level 4 (SA4) region (e.g. `Adelaide - North`,
#' `Barossa - Yorke - Mid North`, `South Australia - Outback`). LGAs not
#' matched by any of the listed groupings are left as `NA` in `Region`.
#'
#' @param data A data frame containing a `LGA` column with South
#'   Australian local government area names (e.g. `"Onkaparinga"`,
#'   `"Mount Gambier"`).
#'
#' @return The input data frame with an added `Region` column giving each
#'   row's ABS SA4 region.
#' @export
map_SA_LGA_to_region <- function(data){

  data <- data |>
    dplyr::mutate(Region = dplyr::case_when(
      # Adelaide - Central and Hills
      LGA %in% c("Adelaide", "Adelaide Hills", "Burnside", "Campbelltown",
                 "Norwood Payneham St Peters", "Prospect", "Unley",
                 "Walkerville", "Mount Barker") ~ "Adelaide - Central and Hills",

      # Adelaide - North
      LGA %in% c("Salisbury", "Playford", "Tea Tree Gully", "Gawler") ~ "Adelaide - North",

      # Adelaide - South
      LGA %in% c("Onkaparinga", "Holdfast Bay", "Mitcham", "Marion") ~ "Adelaide - South",

      # Adelaide - West
      LGA %in% c("Charles Sturt", "Port Adelaide Enfield", "West Torrens") ~ "Adelaide - West",

      # Barossa - Yorke - Mid North
      LGA %in% c("Barossa", "Barunga West", "Clare and Gilbert Valleys", "Copper Coast",
                 "Goyder", "Light", "Mallala", "Adelaide Plains", "Mount Remarkable",
                 "Northern Areas", "Orroroo/Carrieton", "Peterborough",
                 "Port Pirie City and Dists", "Wakefield", "Yorke Peninsula") ~ "Barossa - Yorke - Mid North",

      # South Australia - South East
      LGA %in% c("Alexandrina", "Berri and Barmera", "Grant", "Karoonda East Murray",
                 "Kingston", "Loxton Waikerie", "Mid Murray", "Mount Gambier",
                 "Murray Bridge", "Naracoorte and Lucindale", "Renmark Paringa",
                 "Robe", "Southern Mallee", "Tatiara", "The Coorong",
                 "Victor Harbor", "Wattle Range", "Yankalilla") ~ "South Australia - South East",

      # South Australia - Outback
      LGA %in% c("Ceduna", "Cleve", "Coober Pedy", "Elliston", "Flinders Ranges",
                 "Franklin Harbour", "Kangaroo Island", "Kimba",
                 "Lower Eyre Peninsula", "Port Augusta", "Port Lincoln",
                 "Roxby Downs", "Streaky Bay", "Tumby Bay", "Whyalla",
                 "Wudinna") ~ "South Australia - Outback",

      LGA %in% c("Unincorporated SA") ~ "Unincorporated SA",

      TRUE ~ Region
    ))

}
