#' StatMissing ggproto
#'
#' @keywords internal
StatMissing <- ggproto("StatMissing", Stat,
  compute_group = function(data, scales) {
    if (scales$x$is_discrete()) {
      stop("\"stat_missing()\" does not work with discrete scales.")
    }
    # x-axis range
    xrange = scales$x$range$range
    xlim = scales$x$limits
    if (is.null(xlim)) {
      xlim = xrange
    }
    # don't add missing markers to the left of the range
    newrange = max(xlim[1], xrange[1]):xlim[2]
    missingrange = c(setdiff(newrange, data$x), data$x[is.na(data$y)])
    othercols = names(data)[!(names(data) %in% c("x", "y"))]
    allvals = c(list(x = missingrange), lapply(data[othercols], unique))
    d = expand.grid(allvals)
    if (nrow(d) > 0) {
      d["y"] = 0
    }
    d
  },

  required_aes = c("x", "y")
)

#' Missing Data Markers
#'
#' Add missing data markers to a plot. This function is used
#' to represent years with missing data in column plots.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param nudge_y Vertical adjustment to nudge markers by.
#' @param ...,shape,size,fill,color Other arguments passed on to
#'   [`ggplot2::layer()`].
#' @return a ggplot2 stat.
#'
#' @import ggplot2
#' @export
stat_missing = function(mapping = NULL, data = NULL, geom = "point",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, nudge_y = 0, ..., shape = 24, size = 4,
  fill = "tan2", color = "gray10") {
  if (!missing(nudge_y)) {
    position <- position_nudge(y = nudge_y)
  }
  layer(
    stat = StatMissing, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill = fill, color = color,
    shape = shape, size = size, ...)
  )
}


#' StatLTavg ggproto
#'
#' @keywords internal
StatLTavg <- ggproto("StatLTavg", Stat,
  compute_group = function(data, scales) {
    data.frame(yintercept = mean(data$y))
  },
  required_aes = c("y")
)

#' Long-term Average Line
#'
#' Add a long-term average horixontal line to a plot.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_hline
#' @param ...,color,linetype,size Other arguments passed on to
#'   [`ggplot2::layer()`].
#' @return a ggplot2 stat.
#'
#' @import ggplot2
#' @export
stat_lt_avg <- function(mapping = NULL, data = NULL, geom = "hline",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ..., color = "red", linetype = "dashed",
                       size = 0.9) {
  layer(
    stat = StatLTavg, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, color = color, linetype = linetype,
      size = size, ...)
  )
}
