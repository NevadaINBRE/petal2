new_metric_table <- function(x, metric, sorted) {
  structure(x, class = c("MetricTable", "data.table", "data.frame"),
            metric = metric,
            sorted = sorted)
}

