#' Euclidean distance metric
#' @export
Euclidean <- new_metric(euclidean, 0.0, FALSE, TRUE)

#' Compute the Pearson correlation distance
#' @importFrom coop pcor
pearson <- function(x, y) coop::pcor(x, y)
#' Pearson correlation distance metric
#' @export
Pearson <- new_metric(pearson, 1.0, TRUE, TRUE)

#' Compute the Spearman rank correlation distance
#' @importFrom coop pcor
#' @importFrom data.table frankv
spearman <- function(x, y) {
  coop::pcor(data.table::frankv(x, ties.method = "average"),
             data.table::frankv(y, ties.method = "average"))
}
#' Spearman rank correlation distance metric
#' @export
Spearman <- new_metric(spearman, 1.0, TRUE, TRUE)

#' Compute the Kendall rank correlation distance
#' @importFrom pcaPP cor.fk
kendall <- function(x, y) pcaPP::cor.fk(x, y)
#' Kendall rank correlation distance metric
#' @export
Kendall <- new_metric(kendall, 1.0, TRUE, TRUE)
