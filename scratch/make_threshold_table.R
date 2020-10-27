devtools::load_all()
library(data.table)
library(igraph)

dt <- data.table(INBRE.datasets::dkosma_tobacco)

n <- 1000
m <- as.matrix(dt[1:n,-1])

mm <- metric_matrix(m, "spearman")
mt <- metric_table(mm, sort_decreasing = TRUE)

t0 <- mt$d[n-1]
t1 <- mt$d[150*n]
thresh_seq <- seq(t0, t1, length.out = 6)

am <- ifelse(as.matrix(mm) < thresh_seq[1], 0L, 1L)
g <- graph_from_adjacency_matrix(
  adjmatrix = am,
  mode = "undirected",
  weighted = NULL,
  diag = FALSE
)

D       <- diameter(g)
mean_cc <- transitivity(g, type = "average")  # cluster coefficient
mean_pl <- mean_distance(g, directed = FALSE) # average path length

g_components <- components(g)
n_components <- g_components$no
max_component <- max(g_components$csize)
prop_big_component <- max_component / n

connectivity <- sort(rowSums(am), decreasing = TRUE)
