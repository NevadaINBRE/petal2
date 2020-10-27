library(INBRE.datasets)
library(data.table)
library(igraph)
devtools::load_all()

df <- data.table(dkosma_tobacco)
n <- 10000
m <- t(as.matrix(df[1:n,-1]))
mm <- bigsimr::cor_fast(m, method = "spearman")

am <- ifelse(mm < 0.99, 0, 1)
diag(am) <- 0

g <- graph_from_adjacency_matrix(
  adjmatrix = am,
  mode = "undirected",
  weighted = NULL
)

diameter(g)
mean_distance(g) # == average.path.length(g)
transitivity(g, type = "average")
components(g) # == clusters(g)

