devtools::load_all()
library(data.table)
library(igraph)

dt <- data.table(INBRE.datasets::dkosma_tobacco)

n <- 10000
m <- as.matrix(dt[1:n, -1])
mm <- metric_matrix(m, "spearman")
ts <- simple_threshold_seq(mm, TRUE)

eval_thresh <- sapply(ts, evaluate_threshold, x = mm)
