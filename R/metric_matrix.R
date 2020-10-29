#' Create a metric matrix
#'
#' This is a wrapper for \code{\link[parallelDist]{parDist}} with a few default
#' settings to be specialized for the petal2 package. Namely the diagonal is
#' never stored and the lower triangular part of the distance matrix is used.
#'
#' @param x a numeric matrix where each row is an observation/condition and
#'     each column is a variable/gene
#' @param method the distance measure to be used. A list of all available
#'     distance methods can be found in the details section below and in
#'     \code{\link[parallelDist]{parDist}}
#' @param ... additional parameters which will be passed to the distance methods
#' @return an object of class 'dist'
#' @export
metric_matrix <- function(x, method, ...) {

  method = match.arg(method, choices = dist_methods$all)

  if (method %in% dist_methods$petal) {
    mm <- switch (method,
      pearson  = coop::tpcor(x),
      spearman = coop::tpcor(apply(x, 1, data.table::frankv)),
      kendall  = pcaPP::cor.fk(t(x)),
      mutinformation = infotheo::mutinformation(data.table::data.table(x)),
      bicor = WGCNA::bicor(x),
    )
    mm <- as.dist(mm)
  } else {
    mm <- parallelDist::parDist(x, method, ...)
  }

  mm
}
