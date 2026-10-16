#' build_summary_stat_table
#'
#' Formats one summary statistic of the regression variables as a LaTeX table,
#' with state-pair samples x variables as rows and dwelling/bedroom
#' combinations as columns. Non-control and control datasets are shown in
#' separate panels. Panel A covers the outcomes (New Bonds, Median New Rent);
#' Panel B adds the regression controls.
#'
#' Statistics are computed from the regression-ready files, pooling VIC and the
#' control state within each state pair, as in [build_observation_table()].
#'
#' @param non_control_folder A string. Path to the folder of Excel files for
#'   the non-control samples (one file per state pair and outcome).
#' @param control_folder A string. Path to the folder of Excel files for the
#'   control samples.
#' @param stat A string. One of `"mean"`, `"sd"`, `"median"`, `"min"` or
#'   `"max"`.
#' @param controls Character vector of control columns to summarise in Panel B.
#' @param controls_from A string. The outcome whose files the controls are read
#'   from (e.g. `"Median_Rent"`).
#' @param pair_order Character vector giving the row order of state pairs
#'   within each panel.
#' @param digits Number of decimal places.
#'
#' @returns A `kableExtra` LaTeX table.
#' @export
build_summary_stat_table <- function(non_control_folder,
                                     control_folder,
                                     stat = c("mean", "sd", "median", "min", "max"),
                                     controls      = c("Num_Earners", "Median_Age", "Median_Income"),
                                     controls_from = "Median_Rent",
                                     pair_order    = c("VIC_NSW", "VIC_QLD", "VIC_SA"),
                                     digits        = 2) {

  stat <- match.arg(stat)
  stat_fun <- switch(stat,
                     mean   = mean,
                     sd     = stats::sd,
                     median = stats::median,
                     min    = min,
                     max    = max
  )
  stat_name <- c(mean = "Mean", sd = "Standard deviation", median = "Median",
                 min = "Minimum", max = "Maximum")[[stat]]

  outcomes   <- c("New_Bonds", "Median_Rent")
  var_order  <- unique(c(outcomes, controls))
  var_labels <- c(
    New_Bonds     = "New Bonds",
    Median_Rent   = "Median New Rent",
    Num_Earners   = "Number of Earners",
    Median_Age    = "Median Age",
    Median_Income = "Median Income"
  )

  # Dwelling x Bedrooms combinations -> table columns (row order = column order)
  col_spec <- tibble::tribble(
    ~Dwelling,        ~Bedrooms,   ~group,           ~label,
    "All Properties", "All Sizes", "All Properties", "All BR sizes",
    "Flat",           "1",         "Flat",           "1BR",
    "Flat",           "2",         "Flat",           "2BR",
    "Flat",           "3",         "Flat",           "3BR",
    "House",          "2",         "House",          "2BR",
    "House",          "3",         "House",          "3BR",
    "House",          "4",         "House",          "4BR"
  ) |>
    dplyr::mutate(key = paste(Dwelling, Bedrooms, sep = "_"))

  # Panel A: outcomes only. Panel B: outcomes + controls.
  non_control_stats <- purrr::map(outcomes, \(o)
                                  summarise_variable_in_folder(non_control_folder, o, o, stat_fun)) |>
    dplyr::bind_rows() |>
    dplyr::mutate(Control = FALSE)

  control_stats <- dplyr::bind_rows(
    purrr::map(outcomes, \(o)
               summarise_variable_in_folder(control_folder, o, o, stat_fun)),
    purrr::map(controls, \(v)
               summarise_variable_in_folder(control_folder, controls_from, v, stat_fun))
  ) |>
    dplyr::mutate(Control = TRUE)

  wide <- dplyr::bind_rows(non_control_stats, control_stats) |>
    dplyr::filter(State_Pair %in% pair_order) |>
    dplyr::inner_join(col_spec, by = c("Dwelling", "Bedrooms")) |>
    dplyr::select(Control, State_Pair, Variable, key, Value) |>
    tidyr::pivot_wider(names_from = key, values_from = Value) |>
    dplyr::arrange(Control,
                   factor(State_Pair, levels = pair_order),
                   factor(Variable,   levels = var_order)) |>
    dplyr::mutate(
      # Show the sample name on the first row of each pair only
      Sample   = ifelse(duplicated(paste(Control, State_Pair)), "",
                        sub("_", " & ", State_Pair)),
      Variable = dplyr::coalesce(unname(var_labels[Variable]), Variable)
    )

  # Panel row counts (FALSE sorts before TRUE, so non-control comes first)
  panels <- c(
    "Panel A: Without controls" = sum(!wide$Control),
    "Panel B: With controls"    = sum(wide$Control)
  )

  wide <- dplyr::select(wide, Sample, Variable, dplyr::all_of(col_spec$key))

  # Spanning header: run lengths of consecutive groups
  groups <- rle(col_spec$group)
  header <- c(" " = 2, stats::setNames(groups$lengths, groups$values))

  kableExtra::kbl(
    wide,
    format      = "latex",
    booktabs    = TRUE,
    caption     = paste(stat_name, "of outcomes and controls, by sample"),
    label       = paste0("summary_", stat),
    col.names   = c("Sample", "Variable", col_spec$label),
    align       = c("l", "l", rep("r", nrow(col_spec))),
    digits      = digits,
    format.args = list(big.mark = ",")
  ) |>
    kableExtra::add_header_above(header) |>
    kableExtra::pack_rows(
      index             = panels,
      bold              = FALSE,
      italic            = TRUE,
      indent            = FALSE,
      latex_gap_space   = "0.5em",
      extra_latex_after = "\\addlinespace[2pt]"
    )
}


# HELPERS ----------------------------------------------------------------------

#' summarise_variable_in_folder
#'
#' Applies a summary statistic to one column for each Dwelling x Bedrooms
#' combination, in every Excel file in a folder whose name matches
#' `file_pattern`. Each file is labelled by its state pair, taken from the 4th
#' and 5th underscore-separated words of the file name.
#'
#' @param folder_path A string. Path to the folder of Excel files.
#' @param file_pattern A string. Regular expression selecting the files
#'   (e.g. `"Median_Rent"`).
#' @param variable A string. The column to summarise.
#' @param stat_fun A function taking a numeric vector and `na.rm`.
#'
#' @returns A tibble with columns `State_Pair`, `Dwelling`, `Bedrooms`,
#'   `Value` and `Variable`.
#'
#' @keywords internal
summarise_variable_in_folder <- function(folder_path, file_pattern, variable, stat_fun) {

  file_paths  <- list.files(folder_path, pattern = file_pattern, full.names = TRUE)
  state_pairs <- stringr::word(tools::file_path_sans_ext(basename(file_paths)), 4, 5, sep = "_")

  purrr::map(file_paths, function(file_path) {
    readxl::excel_sheets(file_path) |>
      purrr::map(~ readxl::read_excel(file_path, sheet = .x) |>
                   dplyr::mutate(Bedrooms = as.character(Bedrooms))) |>
      dplyr::bind_rows() |>
      dplyr::group_by(Dwelling, Bedrooms) |>
      dplyr::summarise(Value = stat_fun(.data[[variable]], na.rm = TRUE),
                       .groups = "drop")
  }) |>
    purrr::set_names(state_pairs) |>
    dplyr::bind_rows(.id = "State_Pair") |>
    dplyr::mutate(Variable = variable)
}
