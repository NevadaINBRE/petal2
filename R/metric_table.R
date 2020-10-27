#' @export
metric_table <- function(x, ...) UseMethod("metric_table")

#' Create a sorted metric table from class `dist`
#'
#' @param x An object of class `dist`
#' @param sort_decreasing should the resulting table be sorted in descending order
#' @importFrom data.table data.table setorderv
#' @export
metric_table.dist <- function(x, sort_decreasing) {
  d <- attr(x, "Size")

  l <- labels(x)
  if (is.null(l))
    l <- 1:d

  ij <- utils::combn(length(l), 2)
  dt <- data.table::data.table(
    from = factor(ij[1,], levels = 1:d, labels = l),
    to   = factor(ij[2,], levels = 1:d, labels = l),
    d    = as.numeric(x)
  )

  o <- ifelse(sort_decreasing, -1, 1)
  data.table::setorderv(dt, "d", o)

  dt
}
