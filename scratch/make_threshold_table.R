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
t_est(2000, "sec")


n <- 2000
idx <- sample(1:nrow(dt), size = n, replace = FALSE)
m <- t(as.matrix(dt[idx, -1]))

mm <- metric_matrix(m, "bicor")

ts <- simple_threshold_seq(x = mm, sort_decreasing = TRUE)
sapply(ts, evaluate_threshold, x = mm, method = "less")

# thresh <- ts[5]
# am <- signum_adjacency(as.matrix(x), thresh, method)
# N0 <- nrow(am)
# unconnected_nodes <- unname(which(rowSums(am) == 0))
# if (length(unconnected_nodes) != 0)
#   am <- am[-unconnected_nodes, -unconnected_nodes]
# N1 <- nrow(am)
#
# g <- igraph::graph_from_adjacency_matrix(
#   adjmatrix = am,
#   mode = "undirected",
#   weighted = NULL,
#   diag = FALSE
# )
#
# degree <- igraph::degree(g)
# degree_table <- table(degree)
# degree_df <- as.data.frame(degree_table)
# degree_df$degree <- as.numeric(as.character(degree_df$degree))
# lm(log2(Freq) ~ log2(degree), data = degree_df)
