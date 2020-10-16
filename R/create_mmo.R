#' Create a metric matrix
#'
#' For now, a simple wrapper around `dist`
#' @param x A matrix with the -omics data in the rows and the conditions in the columns
#' @param method The method passed to dist
#' @param ... other parameters passed to dist
#' @export
create_mm <- function(x, method, ...) {
  dist(x, method, ...)
}

#' Create a metric table
#' @param x A matrix with the -omics data in the rows and the conditions in the columns
#' @param method The method passed to dist
#' @param ... other parameters passed to dist
#' @importFrom data.table data.table
#' @export
create_mt <- function(x, method, ...) {
  D <- create_mm(x, method, ...)
  d <- attr(D, "Size")
  L <- labels(D)
  data.table::data.table(from = L[do.call(c, sapply(1:(d-1), function(i) rep(i, d-i)))],
                         to = L[do.call(c, sapply(2:d, function(i) i:d))],
                         d = c(D))
}

#' Create an ordered metric table
#' @param x A matrix with the -omics data in the rows and the conditions in the columns
#' @param method The method passed to dist
#' @param sort_ascending Whether the table should be sorted in ascending order
#' @param ... other parameters passed to dist
#' @importFrom data.table data.table setorderv
#' @export
create_omt <- function(x, method, sort_ascending=TRUE, ...) {
  D <- create_mt(x, method, ...)
  data.table::setorderv(D, "d", ifelse(sort_ascending, 1, -1))
  D
}

