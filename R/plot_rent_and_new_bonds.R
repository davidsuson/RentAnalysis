#' plot_rent_and_new_bonds
#'
#' For every combination of dwelling type and number of bedrooms in `data`,
#' plots the treated state against each control state for median weekly rent
#' and new bonds lodged. One figure is produced per dwelling/bedroom
#' combination.
#'
#' @param data A data frame. Must contain the columns `State`, `Dwelling`,
#'   `Bedrooms`, `Quarter_Start_Date`, `Median_Rent` and `New_Bonds`.
#' @param treated A string. The treated state, plotted in every chart.
#' @param controls A character vector. The states the treated state is
#'   plotted against.
#'
#' @returns A named list of patchwork figures, one per dwelling/bedroom
#'   combination. Names take the form "Dwelling with num_bedroom/s".
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

  names(plots) <- vapply(combos, function(combo) {
    glue::glue("{unique(combo$dwelling)} with {unique(combo$bedroom)} bedroom/s")
  }, character(1))

  return(plots)

}


# HELPERS

#' plot_rent_bonds_segment
#'
#' Builds the figure for one dwelling/bedroom combination. For each control
#' state in `subset_details`, plots median weekly rent and new bonds for the
#' treated state against that control, then arranges all the charts into one
#' figure with a shared legend.
#'
#' @param data A data frame. The full data to subset.
#' @param treated A string. The treated state, plotted in every chart.
#' @param subset_details A data frame with one row per control state, and
#'   the columns:
#'   \itemize{
#'     \item `dwelling`: string. The dwelling type to filter by.
#'     \item `bedroom`: string. The number of bedrooms to filter by.
#'     \item `control`: string. The control state to plot against `treated`.
#'   }
#'   All rows must share the same `dwelling` and `bedroom`.
#' @param palette A named character vector. Maps each state to a colour.
#'
#' @returns A patchwork figure for the given dwelling/bedroom combination. on the same plot.
plot_rent_bonds_segment <- function(data, treated, subset_details, palette) {
  segment_plots <- purrr::pmap(subset_details, function(dwelling, bedroom, control) {
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
  title <- glue::glue("{dwelling} with {bedroom} bedroom/s")

  # You need to create a separate legend as none of the individual legends
  # will contain all the states shown in the combined chart.
  # Therefore, plot all the states together and just extract the legend
  # to reuse for the final plot. Having the same colour palette is important
  # so that the legend matches up with the individual plots in plot_rent
  # and plot_bond.
  legend <- cowplot::get_plot_component(
    plot_rent(dplyr::filter(data, State %in% names(palette)), palette) +
      ggplot2::theme(legend.position = "bottom"),
    "guide-box-bottom",
    return_all = FALSE
  ) |>
    patchwork::wrap_elements()

  final_plot <- arrange_rent_and_bond_plots(plots = segment_plots,
                                            title = title,
                                            legend = legend)


}

#' arrange_rent_and_bond_plots
#'
#' Arranges a list of plots into a two-column figure, with median weekly rent
#' plots in the left column and new bond plots in the right. Individual axis
#' titles are removed and replaced with one header per column and a single
#' "Date" label. A single shared legend is placed at the bottom.
#'
#' @param plots A list of ggplot objects, ordered rent, bond, rent, bond, ...
#'   so that each rent plot is followed by its matching bond plot.
#' @param title A string. The title for the combined figure.
#' @param legend A wrapped_patch. The legend to place below the plots. It
#'   should contain a key and label for every state that appears in any plot.
#'
#' @returns A patchwork figure.
arrange_rent_and_bond_plots <- function(plots, title, legend) {
  charts <- patchwork::wrap_plots(plots, ncol = 2, byrow = TRUE) &
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   legend.position = "none")

  y_titles <- patchwork::wrap_plots(
    convert_text_to_ggplot("Median Weekly Rent ($)"),
    convert_text_to_ggplot("New Bonds (000s)"),
    ncol = 2
  )

  (y_titles / charts / convert_text_to_ggplot("Date") / legend) +
    patchwork::plot_layout(heights = c(0.25, length(plots) / 2, 0.12, 0.12)) +
    patchwork::plot_annotation(title = title,
                               theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))
}

#' convert_text_to_ggplot
#'
#' A small helper that draws a bit of text as its own plot, allowing the text
#' to be used with the rest of ggplot and patchwork function. This is helpful for
#' arranging free standing text.
#'
#' @param text A string. Text to create as its own plot.
#'
#' @returns A text ggplot object containing only the text with no axes or background.
convert_text_to_ggplot <- function(text) {
  ggplot2::ggplot() +
    ggplot2::annotate("text",
                      x = 0,
                      y = 0,
                      label = text) +
    ggplot2::theme_void()
}

#' plot_rent
#'
#' Plots median weekly rent over time, with one line per state.
#'
#' @param data A data frame. Must contain `Quarter_Start_Date`,
#'   `Median_Rent` and `State`.
#' @param palette A named character vector. Maps each state to a colour.
#'
#' @returns A ggplot object.
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

#' plot_bond
#'
#' Plots the number of new bonds lodged over time, with one line per state.
#' The y-axis labels are shown in thousands.
#'
#' @param data A data frame. Must contain `Quarter_Start_Date`, `New_Bonds`
#'   and `State`.
#' @param palette A named character vector. Maps each state to a colour.
#'
#' @returns A ggplot object.
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
