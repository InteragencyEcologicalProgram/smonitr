#' @importFrom ggplot2 aes
#' @export
ggplot2::aes

#' @importFrom ggplot2 expansion
#' @export
ggplot2::expansion

#' @title Round an integer to the nearest multiple
#' @description Used to round a year value to the nearest multiple of a defined
#'     value. This is an internal function used to define x-axis breaks for plots.
#'
#' @param num Integer to round
#' @param mult Mulitple value to round to
#' @keywords internal
#'
#' @return A value rounded to the nearest multiple of the \code{mult} parameter
round_to_mult = function(num, mult) {
  round(num / mult) * mult
}


# WHY IS IT NECESSARY TO IMPORT FROM ggplot2::aes AND ggplot2::expansion AND THEN EXPORT THEM?
