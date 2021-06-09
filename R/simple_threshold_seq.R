#' Generate a sequence of threshold values given a 'dist' object
#'
#' A simple rule is used to get the first and last thresholds. If there are `m`
#' variables, then there are `m(m-1)/2` pairs of variables in the sorted metric
#' table, M. Take the first threshold to be `M[m-1]` and the last threshold to be
#' `M[150m]` \insertCite{petereit2016petal}{petal2}.
#'
#' @param x an object of class `dist`
#' @param sort_decreasing should the resulting table be sorted in descending order
#' @param length.out the number of threshold values to return
#'
#' @examples
#' \dontrun{
#' x <- metric_matrix(dat, "spearman")
#' thresh <- simple_threshold_seq(x, sort_decreasing=TRUE)
#'}
#' @references
#'    \insertAllCited{}
#'
#' @importFrom utils tail
#' @return A sequence of threshold values.
#' @export
simple_threshold_seq <- function(x, sort_decreasing, length.out = 6) {
  m <- attr(x, "Size")

  metric <- sort(as.numeric(x), decreasing = sort_decreasing)

  t0 <- metric[m - 1]
  t1 <- ifelse(m <= 301, tail(metric, 1), metric[150 * m])

  seq(t0, t1, length.out = length.out)
}
