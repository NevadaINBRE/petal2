#' Extract the vicinity network for the genes of interest
#'
#' @param graph The input igraph graph.
#' @param order Integer giving the order of the neighborhood.
#' @param goi The gene names for which the calculation is performed.
#' @param union_graph Whether to preserve the edges in the original graph.
#' @export
make_ego_graph_from_goi <- function(graph, order = 1, goi, union_graph = TRUE) {
  vs <- igraph::V(graph)[igraph::V(graph)$name %in% goi]
  g <- igraph::make_ego_graph(graph, order = order, nodes = vs)
  g <- do.call(what = igraph::union, args = g)

  if (union_graph) {
    vs <- igraph::V(graph)[igraph::V(graph)$name %in% igraph::V(g)$name]
    return(igraph::induced_subgraph(graph, vs))
  } else {
    return(g)
  }

}
