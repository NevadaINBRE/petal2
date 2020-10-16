#' @export
MetricTable <- function(x, ...) UseMethod("MetricTable")

#' Create a Metric Table from class `MetricMatrix`
#'
#' @param x An object of class `MetricMatrix`
#' @param sort should the resulting table be sorted
#' @importFrom data.table data.table setorderv
#' @export
MetricTable.MetricMatrix <- function(x, sort=FALSE) {
  l <- labels(x)
  d <- length(l)


  if (is_symmetric(x)) {
    ij <- utils::combn(length(l), 2)
    dt <-data.table::data.table(
      from = factor(ij[1,], levels = 1:d, labels = l),
      to   = factor(ij[2,], levels = 1:d, labels = l),
      d    = as.numeric(x)
    )
  } else {
    dt <- data.table::data.table(
      from = factor(rep(1:length(l), each = length(l)), levels = 1:d, labels = l),
      to   = factor(rep(1:length(l), times = length(l)), levels = 1:d, labels = l),
      d    = as.numeric(x)
    )
  }

  if (sort) {
    data.table::setorderv(dt, "d", dt_sort_order(x))
  }

  dt
}

#' Create a Metric Table from class `matrix`
#'
#' @param x An object of class `matrix`
#' @param metric An object of class `Metric`
#' @param sort should the resulting table be sorted
#' @export
MetricTable.matrix <- function(x, metric, sort=FALSE) {
  MetricTable(MetricMatrix(x, metric), sort)
}
