#' plot_rent_and_new_bonds
#'
#' For every combination of dwelling type and number of bedrooms, plots the
#' treated state against each control state for median weekly rent and new
#' bonds lodged. Data are read from every Excel file in `folder` (one file
#' per state pair and outcome, one sheet per dwelling/bedroom combination).
#' One figure is produced per dwelling/bedroom combination.
#'
#' @param folder A string. Path to the folder of state-level Excel files.
#'   File names must contain `"Median_Rent"` or `"New_Bonds"`, and every
#'   sheet must contain the columns `State`, `Dwelling`, `Bedrooms`,
#'   `Quarter_Start_Date` and the outcome column.
#' @param treated A string. The treated state, plotted in every chart.
#' @param controls A character vector. The states the treated state is
#'   plotted against.
#' @param period A string. Time unit to plot: `"quarter"` (default), `"half"`
#'   (half-years: Jan-Jun, Jul-Dec) or `"year"` (calendar years). For longer
#'   periods, new bonds are summed and median rent is the mean of the
#'   quarterly medians. Periods missing any quarter are dropped.
#'
#' @returns A named list of patchwork figures, one per dwelling/bedroom
#'   combination. Names take the form "Dwelling with num_bedroom/s".
#'
#' @seealso [load_rent_and_bond_data()]
plot_rent_and_new_bonds <- function(folder, treated, controls,period = c("quarter", "half", "year")) {

  period <- match.arg(period)

  data <- load_rent_and_bond_data(folder) |>
    aggregate_to_period(period)

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

  plots

}

# HELPERS

#' load_rent_and_bond_data
#'
#' Reads every Median_Rent and New_Bonds Excel file in a folder (all sheets),
#' removes duplicate rows for states that appear in more than one state-pair
#' file, and joins the two outcomes into one data frame.
#'
#' @param folder A string. Path to the folder of Excel files.
#'
#' @returns A tibble with one row per State x Dwelling x Bedrooms x
#'   Quarter_Start_Date and the columns `State`, `Dwelling`, `Bedrooms`,
#'   `Quarter_Start_Date`, `Median_Rent` and `New_Bonds`.
load_rent_and_bond_data <- function(folder) {

  keys <- c("State_Pair", "State", "Dwelling", "Bedrooms", "Quarter_Start_Date")

  rent  <- read_outcome_files(folder, "Median_Rent", keys)
  bonds <- read_outcome_files(folder, "New_Bonds",   keys)

  dplyr::full_join(rent, bonds, by = keys)
}

#' read_outcome_files
#'
#' Reads every sheet of every Excel file in `folder` whose name contains
#' `outcome`, stacks them, keeps only the key columns and the outcome, and
#' removes duplicate rows (e.g. VIC appearing in every state-pair file).
#'
#' @param folder A string. Path to the folder of Excel files.
#' @param outcome A string. The outcome column, also used to select files.
#' @param keys A character vector. Columns identifying one observation.
#'
#' @returns A tibble with the `keys` columns and the `outcome` column.
read_outcome_files <- function(folder, outcome, keys) {

  file_paths  <- list.files(folder, pattern = outcome, full.names = TRUE)
  state_pairs <- stringr::word(tools::file_path_sans_ext(basename(file_paths)), 4, 5, sep = "_")

  purrr::map2(file_paths, state_pairs, function(file_path, pair) {
    readxl::excel_sheets(file_path) |>
      purrr::map(~ readxl::read_excel(file_path, sheet = .x) |>
                   # "All Sizes" makes some sheets text and others numeric
                   dplyr::mutate(Bedrooms = as.character(Bedrooms))) |>
      dplyr::bind_rows() |>
      dplyr::mutate(State_Pair = pair)
  }) |>
    dplyr::bind_rows() |>
    dplyr::select(dplyr::all_of(c(keys, outcome)))
}

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
      dplyr::filter(State_Pair == paste(treated, control, sep = "_"),
                    Dwelling == dwelling,
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

#' aggregate_to_period
#'
#' Collapses quarterly data to half-years or calendar years. New bonds are
#' summed and median rent is averaged across the quarters in each period.
#' Periods that don't contain every quarter are dropped.
#'
#' `Quarter_Start_Date` is overwritten with the period's start date so the
#' plotting helpers work unchanged.
#'
#' @param data A data frame from [load_rent_and_bond_data()].
#' @param period A string. `"quarter"`, `"half"` or `"year"`.
#'
#' @returns A tibble with the same columns as `data`.
aggregate_to_period <- function(data, period) {

  if (period == "quarter") return(data)

  quarters_per_period <- c(half = 2, year = 4)[[period]]

  data |>
    dplyr::mutate(
      date        = as.Date(Quarter_Start_Date),
      year        = as.integer(format(date, "%Y")),
      month       = as.integer(format(date, "%m")),
      start_month = if (period == "half") ifelse(month <= 6, 1L, 7L) else 1L,
      Quarter_Start_Date = as.Date(sprintf("%d-%02d-01", year, start_month))
    ) |>
    dplyr::group_by(State_Pair, State, Dwelling, Bedrooms, Quarter_Start_Date) |>
    dplyr::summarise(
      n_quarters  = dplyr::n(),
      Median_Rent = mean(Median_Rent),
      New_Bonds   = sum(New_Bonds),
      .groups     = "drop"
    ) |>
    dplyr::filter(n_quarters == quarters_per_period) |>
    dplyr::select(-n_quarters)
}
