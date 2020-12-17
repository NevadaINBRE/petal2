devtools::load_all()
library(igraph)

m <- SampleData_small
m <- m[,complete.cases(t(m))]

mm <- metric_matrix(m, "bicor")
am <- signum_adjacency(mm, 0.9)

g <- graph_from_adjacency_matrix(
  adjmatrix = am,
  mode = "undirected",
  weighted = NULL,
  diag = FALSE
)
plot(g)
