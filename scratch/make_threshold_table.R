devtools::load_all()
library(data.table)
library(igraph)

dt <- data.table(INBRE.datasets::dkosma_tobacco_rlog)

# Time to completion:
# t(n) ~ 2n^2 [microseconds]
# t(30K) ~ 30 [minutes]
# t(40K) ~ 53 [minutes]
t_est <- function(n, units = c("us", "ms", "sec", "min", "hr")) {
  units <- match.arg(units)
  x <- 2*n^2
  switch (units,
    us  = paste(round(x, 0), "microseconds"),
    ms  = paste(round(x / 1e3, 0), "milliseconds"),
    sec = paste(round(x / 1e6, 0), "seconds"),
    min = paste(round(x / 1e6 / 60, 1), "minutes"),
    hr  = paste(round(x / 1e6 / 60 / 60, 1), "hours")
  )
}
t_est(nrow(dt), "min")


n <- 11342
idx <- sample(1:nrow(dt), size = n, replace = FALSE)
m <- as.matrix(dt[idx, -1])

system.time({
  mm <- metric_matrix(m, "spearman")
  ts <- simple_threshold_seq(x = mm, sort_decreasing = TRUE)
  thresh_table_10K_SP <- sapply(ts, evaluate_threshold, x = mm, method = "less")
})
thresh_table_10K_SP

system.time({
  mm <- metric_matrix(m, "pearson")
  ts <- simple_threshold_seq(x = mm, sort_decreasing = TRUE)
  thresh_table_10K_PE <- sapply(ts, evaluate_threshold, x = mm, method = "less")
})
thresh_table_10K_PE

system.time({
  mm <- metric_matrix(m, "euclidean")
  ts <- simple_threshold_seq(x = mm, sort_decreasing = FALSE)
  thresh_table_10K_EU <- sapply(ts, evaluate_threshold, x = mm, method = "greater")
})
thresh_table_10K_EU

