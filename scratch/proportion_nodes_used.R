devtools::load_all()
library(data.table)
library(igraph)

dt <- data.table(INBRE.datasets::dkosma_tobacco_rlog)

n <- 1000
idx <- sample(1:nrow(dt), size = n, replace = FALSE)
m <- as.matrix(dt[idx, -1])

mm <- metric_matrix(m, "spearman")
am <- signum_adjacency(as.matrix(mm), 0.9, "less")
unconnected_nodes <- unname(which(rowSums(am) == 0))
am2 <- am[-unconnected_nodes, -unconnected_nodes]

g <- igraph::graph_from_adjacency_matrix(
  adjmatrix = am2,
  mode = "undirected",
  weighted = NULL,
  diag = FALSE
)

g_components <- igraph::components(g)
n_components <- g_components$no
max_component <- max(g_components$csize)

# Proportion Big Components
max_component / nrow(am2)

# Proportion Used
nrow(am2) / nrow(am)

