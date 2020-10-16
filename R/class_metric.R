# Constructors (internal use)
new_metric <- function(.f, sort_decreasing, self_dist, is_symmetric, is_triangular) {
  classes <- "PreMetric"
  if (is_symmetric) classes <- c("SemiMetric", classes)
  if (is_triangular) classes <- c("Metric", classes)

  structure(.f, class = classes,
            self_dist = self_dist,
            sort_decreasing = sort_decreasing)
}


# Validator (Go along with helpers)
validate_metric <- function(m) {

  f <- unclass(m)
  stopifnot(is.function(f))
  self_dist <- attr(m, "self_dist")

  x <- c(0, 3.14, 1, 2.718)
  y <- c(8, 6.75, 3, 0.9)

  is_self_similar <- all.equal(m(x, x), self_dist)
  is_symmetric <- all.equal(m(x, y), m(y, x))

  if (!is_self_similar) {
    stop(
      "The reported self_similar value must match the computed value",
      call. = FALSE
    )
  }

  if (is_SemiMetric(m) && !is_symmetric) {
    stop(
      "A SemiMetric must be symmetric. I.e. f(x,y) == f(y,x) for all x,y",
      call. = FALSE
    )
  }

  m
}


# Helpers (external constructors)
PreMetric <- function(.f, sort_decreasing, self_dist = .f(1, 1)) {
  validate_metric(new_metric(.f, sort_decreasing, self_dist, FALSE, FALSE))
}
SemiMetric <- function(.f, sort_decreasing, self_dist = .f(1, 1)) {
  validate_metric(new_metric(.f, sort_decreasing, self_dist, TRUE, FALSE))
}
Metric <- function(.f, sort_decreasing, self_dist = .f(1, 1)) {
  validate_metric(new_metric(.f, sort_decreasing, self_dist, TRUE, TRUE))
}


# Utilities
is_PreMetric  <- function(x) "PreMetric" %in% class(x)
is_SemiMetric <- function(x) "SemiMetric" %in% class(x)
is_Metric     <- function(x) "Metric" %in% class(x)


# Metric Definitions

#' Euclidean distance metric
#' @export
Euclidean <- Metric(euclidean, sort_decreasing = FALSE, self_dist = 0.0)

#' Compute the Pearson correlation distance
#' @importFrom coop pcor
#' @export
pearson <- function(x, y) coop::pcor(x, y)
#' Pearson correlation distance metric
#' @export
Pearson <- Metric(pearson, sort_decreasing = TRUE, self_dist = 1.0)

#' Compute the Spearman rank correlation distance
#' @importFrom coop pcor
#' @importFrom data.table frankv
#' @export
spearman <- function(x, y) {
  coop::pcor(data.table::frankv(x, ties.method = "average"),
             data.table::frankv(y, ties.method = "average"))
}
#' Spearman rank correlation distance metric
#' @export
Spearman <- Metric(spearman, sort_decreasing = TRUE, self_dist = 1.0)

#' Compute the Kendall rank correlation distance
#' @importFrom pcaPP cor.fk
#' @export
kendall <- function(x, y) pcaPP::cor.fk(x, y)
#' Kendall rank correlation distance metric
#' @export
Kendall <- Metric(kendall, sort_decreasing = TRUE, self_dist = 1.0)
