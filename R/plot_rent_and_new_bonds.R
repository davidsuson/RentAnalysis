plot_rent_and_new_bonds <- function(data, treated, controls) {
  combos <- data |>
    dplyr::filter(State != treated) |>
    dplyr::rename(dwelling = "Dwelling", bedroom = "Bedrooms") |>
    dplyr::distinct(dwelling, bedroom) |>
    tidyr::crossing(control = controls) |>
    dplyr::group_split(dwelling, bedroom)

  states  <- c(treated, controls)
  palette <- stats::setNames(scales::hue_pal()(length(states)), states)

  plots <- lapply(combos, function(combo) {
    plot_rent_bonds_segment(data,
                            treated,
                            subset_details = combo,
                            palette = palette)

  })


}


# HELPERS

#' plot_rent_bonds_segment
#'
#' For each row of `subset_details`, plots median weekly rent and new bonds for the
#' treated state against that row's control state.
#'
#' @param data A data frame. Contains the data to subset.
#' @param treated A string. The treated state to plot.
#' @param subset_details A dataframe. Contains the details to filter the data by. Should have the columns:
#' 1. Control. A string. The control state to plot.
#' 2. Dwelling. A string. The dwelling type to subset the data by.
#' 3. Bedroom. A string. The number of bedrooms to subset the data by.
#' @param palette Named vector. Maps each state to a color.
#'
#' @returns Plots of median weekly rent and control by dwelling and bedroom with the treated and control states on the same plot.
plot_rent_bonds_segment <- function(data, treated, subset_details, palette) {
  segment_plots <-   purrr::pmap(subset_details, function(dwelling, bedroom, control) {
    segment_data <- data |>
      dplyr::filter(Dwelling == dwelling,
                    State %in% c(treated, control),
                    Bedrooms == bedroom)

    list(plot_rent(segment_data, palette),
         plot_bond(segment_data, palette))
  }) |>
    purrr::list_flatten()

  dwelling <- unique(subset_details$dwelling)
  bedroom <- unique(subset_details$bedroom)

  # A small helper that draws a bit of text as its own plot
  label_strip <- function(text) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0, y = 0, label = text) +
      ggplot2::theme_void()
  }

  dwelling <- unique(subset_details$dwelling)
  bedroom  <- unique(subset_details$bedroom)

  charts <- patchwork::wrap_plots(segment_plots, ncol = 2, byrow = TRUE) &
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   legend.position = "none")

  legend <- cowplot::get_plot_component(
    plot_rent(dplyr::filter(data, State %in% names(palette)), palette) +
      ggplot2::theme(legend.position = "bottom"),
    "guide-box-bottom",
    return_all = FALSE
  ) |>
    patchwork::wrap_elements()

  y_titles <- patchwork::wrap_plots(label_strip("Median Weekly Rent ($)"), label_strip("New Bonds (000s)"), ncol = 2)

  (y_titles / charts / label_strip("Date") / legend) +
    patchwork::plot_layout(heights = c(0.25, nrow(subset_details), 0.12, 0.12)) +
    patchwork::plot_annotation(title = glue::glue("{dwelling} with {bedroom} bedroom/s"),
                               theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))

}

#' plot_rent
#'
#' @param data Data to plot.
#' @param palette Named vector. Maps each state to a color.
#'
#' @returns Median weekly rent plotted by quarter start date and grouped by state.
plot_rent <- function(data, palette) {
  rent_plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = Quarter_Start_Date,
      y = Median_Rent,
      colour = State,
      group = State
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = palette) +
    ggplot2::labs(x = "Date", y = "Median Weekly Rent ($)", colour = "State") +
    ggplot2::theme_minimal()
}

#' plot_bonds
#'
#' @param data Data to plot.
#' @param palette Named vector. Maps each state to a color.
#'
#' @returns New bonds lodged plotted by quarter start date and grouped by state.
plot_bond <- function(data, palette) {
  bond_plot <- ggplot2::ggplot(data,
                               ggplot2::aes(
                                 x = Quarter_Start_Date,
                                 y = New_Bonds,
                                 colour = State,
                                 group = State
                               )) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = palette) +
    ggplot2::labs(x = "Date", y = "New Bonds (000s)", colour = "State") +
    ggplot2::scale_y_continuous(labels = scales::label_number(scale = 1e-3)) +
    ggplot2::theme_minimal()
}
