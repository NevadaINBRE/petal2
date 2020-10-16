#' @export
is_symmetric <- function(x) UseMethod("is_symmetric")
#' @export
is_symmetric.Metric       <- function(x) attr(x, "is_symmetric")
#' @export
is_symmetric.MetricMatrix <- function(x) is_symmetric(get_metric(x))



#' @export
self_distance <- function(x) UseMethod("self_distance")
#' @export
self_distance.Metric <- function(x) attr(x, "self_distance")
#' @export
self_distance.MetricMatrix <- function(x) self_distance(get_metric(x))



#' @export
get_metric <- function(x) UseMethod("get_metric")
#' @export
get_metric.MetricMatrix <- function(x) attr(x, "metric")



#' @export
labels.MetricMatrix <- function(x) attr(x, "labels")



#' @export
is_Metric     <- function(x) class(x) == "Metric"



#' @export
dt_sort_order <- function(x) UseMethod("dt_sort_order")
#' @export
dt_sort_order.Metric <- function(x) ifelse(attr(x, "sort_decreasing"), -1L, 1L)
#' @export
dt_sort_order.MetricMatrix <- function(x) dt_sort_order(get_metric(x))
