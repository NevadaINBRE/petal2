#' @export
is_symmetric <- function(x) {
  UseMethod("is_symmetric")
}
is_symmetric.Metric       <- function(x) attr(x, "is_symmetric")
is_symmetric.MetricMatrix <- function(x) is_symmetric(get_metric(x))



#' @export
self_distance <- function(x) {
  UseMethod("self_distance")
}
self_distance.Metric <- function(x) attr(x, "self_distance")
self_distance.MetricMatrix <- function(x) self_distance(get_metric(x))



#' @export
get_metric <- function(x) {
  UseMethod("get_metric")
}
get_metric.MetricMatrix <- function(x) attr(x, "metric")



#' @export
labels.MetricMatrix <- function(x) attr(x, "labels")


is_Metric     <- function(x) class(x) == "Metric"
dt_sort_order <- function(x) ifelse(attr(x, "sort_decreasing"), -1L, 1L)
