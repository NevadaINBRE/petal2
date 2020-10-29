devtools::load_all()
library(data.table)
library(infotheo)

dt <- data.table(INBRE.datasets::dkosma_tobacco_vsd)

n <- 4000

idx <- sample(1:nrow(dt), size = n, replace = FALSE)
m <- t(as.matrix(dt[idx, -1]))
m <- discretize(m)

mm <- metric_matrix(m, "mutinformation")
unique(as.numeric(mm))
