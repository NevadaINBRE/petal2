new_metric <- function(.f, self_dist, sort_decreasing, is_symmetric) {
  structure(.f, class = "Metric",
            self_dist = self_dist,
            sort_decreasing = sort_decreasing,
            is_symmetric = is_symmetric)
}




validate_metric <- function(m) {

  f <- unclass(m)
  stopifnot(is.function(f))
  stopifnot(is.logical(attr(m, "sort_decreasing")))
  stopifnot(is.logical(attr(m, "is_symmetric")))

  x <- c(0, 3.14, 1, 2.718)
  y <- c(8, 6.75, 3, 0.900)

  stopifnot(all.equal(self_distance(m), m(x,x)))
  stopifnot(all.equal(self_distance(m), m(y,y)))

  # TODO: Determine how best to check for symmetry

  m
}




#' Create a new metric
#'
#' In this package, a metric is used to generally mean the (dis)similarity
#'     between two vectors of genes or proteins.
#' @param .f A function that takes in two vectors
#' @param self_dist The distance between a vector and itself. For the Euclidean
#'     metric, the value is `0.0`, and for correlation metrics, the value is `1.0`
#' @param sort_decreasing Should values be sorted in ascending order (smaller
#'     values implying more similarity) or in descending order (larger values
#'     implying more similarity)
#' @param is_symmetric Is the metric `f(x, y)` the same as `f(y, x)`
#' @export
Metric <- function(.f, self_dist = .f(1, 1), sort_decreasing, is_symmetric) {
  validate_metric(new_metric(.f, self_dist, sort_decreasing, is_symmetric))
}
