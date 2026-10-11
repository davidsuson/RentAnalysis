#' export_combined_event_plots
#'
#' Saves one PNG per state pair and control setting, combining the stored
#' event-study plots for every dwelling/bedroom combination into a grid with
#' shared axis titles and a shared legend.
#'
#' @param results The list returned by [run_did_regressions()]. Each sheet's
#'   results must contain an `event_plot` element.
#' @param outcome A string. The outcome name (e.g. `"Median_Rent"`), used in
#'   the subtitles and file names.
#' @param output_folder A string. Folder to save the PNGs in. Created if it
#'   doesn't exist.
#' @param x_gap Integer. Show an x-axis label every `x_gap` quarters.
#' @param width,height Numeric. Figure size in inches.
#' @param dpi Numeric. Resolution of the PNG.
#'
#' @returns `NULL`, invisibly. Called for its side effect of writing files.
#' @export
export_combined_event_plots <- function(results, outcome, output_folder,
                                        x_gap = 3, width = 14, height = 7,
                                        dpi = 300) {

  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

  control_labels <- c(non_control = "without controls", control = "with controls")

  # Sheet name -> panel title, in display order
  panels <- c(
    "All Properties_All Sizes" = "All Properties, all sizes",
    "Flat_1"  = "Flat 1BR",  "Flat_2"  = "Flat 2BR",  "Flat_3"  = "Flat 3BR",
    "House_2" = "House 2BR", "House_3" = "House 3BR", "House_4" = "House 4BR"
  )

  # Shared axis titles, drawn as text-only plots
  y_title <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = "ATT", angle = 90, size = 5) +
    ggplot2::theme_void()

  x_title <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = "Quarters relative to treatment", size = 5) +
    ggplot2::theme_void()

  purrr::iwalk(results, function(pairs, control_key) {
    purrr::iwalk(pairs, function(sheets, pair) {

      sheets <- sheets[intersect(names(panels), names(sheets))]

      # Modify each stored plot
      plots <- purrr::imap(sheets, function(res, sheet) {
        sparse_x_axis(res$event_plot, x_gap) +
          ggplot2::labs(title = panels[[sheet]], x = NULL, y = NULL) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 11, face = "bold", colour = "black")
          )
      })

      grid <- patchwork::wrap_plots(plots, ncol = 4)

      combined <- patchwork::wrap_plots(
        y_title, grid,
        patchwork::plot_spacer(), x_title,
        ncol    = 2,
        widths  = c(0.03, 1),
        heights = c(1, 0.05),
        guides  = "collect"
      ) +
        patchwork::plot_annotation(
          title    = "Average Effect by Length of Exposure",
          subtitle = sprintf("%s: %s, %s",
                             gsub("_", " ", outcome),
                             sub("_", " & ", pair),
                             control_labels[[control_key]]),
          theme    = ggplot2::theme(
            plot.title    = ggplot2::element_text(size = 16, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 12)
          )
        ) &
        ggplot2::theme(legend.position = "bottom")

      file_name <- paste(outcome, pair, control_key, "event_study_plots", sep = "_")

      ggplot2::ggsave(
        file.path(output_folder, paste0(file_name, ".png")),
        combined, width = width, height = height, dpi = dpi
      )
    })
  })

  invisible(NULL)
}

# HELPERS ----------------------------------------------------------------------

#' sparse_x_axis
#'
#' Thins the x-axis labels of an existing plot so only every `gap`-th event
#' time (multiples of `gap`, including 0) is labelled. Works whether the plot
#' has a discrete or continuous x scale.
#'
#' @param plot A `ggplot` object, e.g. from [did::ggdid()].
#' @param gap Integer. Label every `gap` quarters.
#'
#' @returns The modified `ggplot`.
#'
#' @keywords internal
sparse_x_axis <- function(plot, gap = 3) {

  x_scale <- plot$scales$get_scales("x")

  new_scale <- if (!is.null(x_scale) && inherits(x_scale, "ScaleDiscrete")) {
    ggplot2::scale_x_discrete(breaks = function(x) x[as.numeric(x) %% gap == 0])
  } else {
    ggplot2::scale_x_continuous(
      breaks = function(lims) seq(gap * ceiling(lims[1] / gap), lims[2], by = gap)
    )
  }

  # Replacing ggdid's x scale prints a "scale already present" message
  suppressMessages(plot + new_scale)
}
