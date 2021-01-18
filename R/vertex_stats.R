#' Return basic properties of all vertices in a graph
#'
#' Properties include:
#' * vertex number
#' * vertex label (if it exists)
#' * cluster coefficient
#' * degree
#' * eccentricity
#' * mean distance
#' * median distance
#'
#' @param graph An igraph graph.
#' @export
vertex_stats <- function(graph) {

  df <- data.frame(vertex = as.numeric(V(graph)))

  # If a field is empty (NULL), then no column gets added
  df$label        <- V(graph)$label
  df$cluster_coef <- igraph::transitivity(graph, type = "local")
  df$degree       <- igraph::degree(graph)
  df$eccentricity <- igraph::eccentricity(graph)

  dists <- igraph::distances(g)
  df$mean_distance   <- colMeans(dists)
  df$median_distance <- apply(dists, 2, median)

  df
}
