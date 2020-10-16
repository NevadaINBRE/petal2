library(INBRE.datasets)
library(data.table)
library(petal2)

df <- data.table(dkosma_tobacco)
n <- 100
metric <- Pearson
m <- t(as.matrix(df[1:n,-1]))
mm <- MetricMatrix(m, metric)
dt_sort_order(metric)
self_distance(metric)

get_metric(mm)
is_symmetric(metric)
is_symmetric(mm)

is_Metric(metric)


mt_m  <- MetricTable(m, Pearson, FALSE)
mt_mm <- MetricTable(mm, TRUE)
