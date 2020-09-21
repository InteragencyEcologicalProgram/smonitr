#' Custom formatting for IEP Seasonal Monitoring Report figures
#'
#' This custom theme builds off of the [`ggplot2::theme_bw()`]
#' theme to remove plot gridlines and modify the text size of
#' various plot elements.
#'
#' @return A ggplot theme.
#'
#' @import ggplot2
#' @export
smr_theme = function() {
  theme_bw() + theme(
    # Define text size of tick labels
    axis.text = element_text(
      size = 9
    ),
    # Define text size and face of axes labels
    axis.title = element_text(
      size = 10,
      face = "plain"
    ),
    # Remove panel grid lines
    panel.grid = element_blank(),
    # Adjust the margin dimensions so nothing is cut off
    plot.margin = unit(
      c(0.25, 0.6, 0.1, 0.4), #top, right, bottom, left
      units = "cm"
    ),
    # Define text size and justification of plot title
    plot.title = element_text(
      size = 20,
      vjust = 1,
      hjust = 0.5
    ),
    # Define text size and face of legend item labels
    legend.text = element_text(
      size = 9,
      face = "plain"
    ),
    # Define text size of legend title
    legend.title = element_text(
      size = 10
    ),
    # Define legend position
    legend.position = "top"
  )
}


#' Custom x-axis for IEP Seasonal Monitoring Report figures
#'
#' Standardizes the x-axis limits and breaks of a ggplot.
#'
#' @param report_year The user-defined report year for the Seasonal
#'   Monitoring Report. Must be an integer.
#' @param type The scale type to use. `type = "all"` defines the
#'   minimum x-axis limit as 1966, which corresponds to the earliest
#'   survey shown in the fall season report (Fall Midwater Trawl).
#'   `type = "recent"` defines the minimum x-axis limit as 2004, which
#'   corresponds to the Pelagic Organism Decline.
#' @param season The season for the Seasonal Monitoring Report.
#'   Must be one of the following: `"winter"`, `"spring"`,
#'   `"summer"`, `"fall"`, or `"annual"`.
#' @param interval (optional) override the x-axis break interval.
#'   By default, `type = "all"` results in a break interval of 10 years
#'   while `type = "recent"` results in a break interval of 5 years.
#' @param start_year (optional) override the x-axis lower limit.
#' @return A ggplot continuous x-axis scale.
#'
#' @import ggplot2
#' @importFrom utils head tail
#' @export
smr_x_axis = function(report_year, type = c("all", "recent"),
  season = c("annual", "winter", "spring", "summer", "fall"),
  interval, start_year) {
  # argument checking
  type = match.arg(type, c("all", "recent"))
  season = match.arg(season, c("annual", "winter", "spring",
    "summer", "fall"))
  if (missing(start_year)) {
    start_year = switch(type,
      "all" = 1966L,
      "recent" = 2004L
    )
  }
  if (missing(interval)) {
    interval = switch(type,
      "all" = 10L,
      "recent" = 5L
    )
  } else {
    interval = as.integer(interval)
  }
  # define the axis title
  title = switch(season,
    "winter" = "Year (December - February)",
    "spring" = "Year (March - May)",
    "summer" = "Year (June - August)",
    "fall" = "Year (September - November)",
    "annual" = "Year"
  )
  # define the breaks
  year_breaks <- seq.int(
    from = round_to_mult(start_year, interval),
    to = round_to_mult(report_year, interval),
    by = interval
  )
  ### ensure that the minimum and maximum years are labeled
  all_breaks = c(start_year, year_breaks, report_year)
  all_breaks = unique(all_breaks[all_breaks <= report_year])
  if (diff(tail(all_breaks, 2)) < 0.5 * interval) {
    all_breaks = all_breaks[-(length(all_breaks) - 1)]
  }
  if (diff(head(all_breaks, 2)) < 0.5 * interval) {
    all_breaks = all_breaks[-2]
  }

  # return the scale
  scale_x_continuous(title, breaks = all_breaks,
    limits = c(start_year - 0.5, report_year + 0.5))
}


#' Custom y-axis for IEP Seasonal Monitoring Report figures
#'
#' Standardizes the y-axis a ggplot. This is really just a wrapper
#' around[`ggplot2::scale_y_continuous()`] with a default setting
#' for the `expand` argument to reduce whitespace.
#'
#' @inheritParams ggplot2::scale_y_continuous
#' @param ... Other arguments passed on to [`ggplot2::scale_y_continuous()`]
#' @return A ggplot continuous x-axis scale.
#'
#' @import ggplot2
#' @export
smr_y_axis = function(expand = expansion(mult = c(0, 0.05)), ...) {
  scale_y_continuous(..., expand = expand)
}
