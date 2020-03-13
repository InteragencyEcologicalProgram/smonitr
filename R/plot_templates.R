#' Column plot template for IEP Seasonal Monitoring Report figures
#'
#' A template function for producing column plots for the IEP
#' Seasonal Monitoring Report.
#'
#' @inheritParams ggplot2::ggplot
#' @inheritParams smr_x_axis
#' @param ... other arguments to pass to `[geom_col()]`
#'
#' @import rlang
#' @importFrom purrr map_chr
#' @import ggplot2
#' @export
standard_column_plot = function(data, mapping, report_year,
  season = c("annual", "winter", "spring", "summer", "fall"),
  type = c("recent", "all"), ...) {
  # define the x-axis
  xaxis = smr_x_axis(report_year = report_year, type = type,
    season = season)
  # check that the columns specified in mapping exist in data
  if (!all(map_chr(unname(mapping), as_name) %in% names(data))) {
    stop("Argument \"data\" does not contain columns: ",
      paste(shQuote(setdiff(map_chr(unname(mapping), as_name),
        names(data))), collapse = ", "))
  }
  # column type checking
  for (col in mapping[c("x", "y")]) {
    if (!is.numeric(data[[as_name(col)]])) {
      stop("Column ", shQuote(as_name(col)), " must be numeric.")
    }
  }
  # convert year to integer
  data[as_name(mapping$x)] = as.integer(data[[as_name(mapping$x)]])
  # construct the plot
  ggplot(data, mapping) +
    smr_theme() +
    xaxis +
    smr_y_axis() +
    geom_col(...) +
    stat_missing(nudge_y = 0) +
    stat_lt_avg()
}
