library(INBRE.datasets)
library(data.table)
devtools::load_all()

df <- data.table(dkosma_tobacco)
n <- 1000
metric <- Euclidean
m <- t(as.matrix(df[1:n,-1]))

system.time({
  mm <- MetricMatrix(m, metric)
})

system.time({
  cmm <- as.numeric(.Call(`_petal2_mmsym`, m, metric))
})

system.time({
  dm <- dist(t(m))
})

dt_sort_order(metric)
self_distance(metric)

get_metric(mm)
is_symmetric(metric)
is_symmetric(mm)

is_Metric(metric)


mt_m  <- MetricTable(m, Pearson, FALSE)
mt_mm <- MetricTable(mm, TRUE)
