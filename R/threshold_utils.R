#' Generate a sequence of threshold values given a 'dist' object
#'
#' @param x an object of class `dist`
#' @param sort_decreasing should the resulting table be sorted in descending order
#' @param length.out the number of threshold values to return
#' @importFrom utils tail
#' @export
simple_threshold_seq <- function(x, sort_decreasing, length.out = 6) {
  m <- attr(x, "Size")

  metric <- sort(as.numeric(x), decreasing = sort_decreasing)

  t0 <- metric[m - 1]
  t1 <- ifelse(m <= 301, tail(metric, 1), metric[150 * m])

  seq(t0, t1, length.out = length.out)
}


#' Evaluate the properties of a graph determined by a threshold value
#'
#' @param x an object of class `dist`
#' @param thresh a cutoff value to create the adjacency matrix
#' @param method which side of the cutoff value to set to 0
#' @importFrom stats coef lm median
#' @export
evaluate_threshold <- function(x, thresh, method = "less") {

  am <- signum_adjacency(as.matrix(x), thresh, method)
  N0 <- nrow(am)
  unconnected_nodes <- unname(which(rowSums(am) == 0))
  if (length(unconnected_nodes) != 0)
    am <- am[-unconnected_nodes, -unconnected_nodes]
  N1 <- nrow(am)

  g <- igraph::graph_from_adjacency_matrix(
    adjmatrix = am,
    mode = "undirected",
    weighted = NULL,
    diag = FALSE
  )

  D <- igraph::diameter(g)
  mean_cc <- igraph::transitivity(g, type = "average") # cluster coefficient
  mean_pl <- igraph::mean_distance(g, directed = FALSE) # average path length

  g_components <- igraph::components(g)
  n_components <- g_components$no
  max_component <- max(g_components$csize)

  proportion_nodes_used <- N1 / N0
  proportion_big_components <- max_component / N1


  degree           <- igraph::degree(g)
  degree_table     <- table(degree)
  degree_df        <- as.data.frame(degree_table)
  degree_df$degree <- as.numeric(as.character(degree_df$degree))
  degree_fit       <- lm(log2(Freq) ~ log2(degree), data = degree_df)
  degree_summary   <- summary(degree_fit)
  degree_power     <- -unname(coef(degree_fit)[2])

  list(
    threshold                 = thresh,
    diameter                  = D,
    mean_path_length          = mean_pl,
    mean_cluster_coefficient  = mean_cc,
    number_of_components      = n_components,
    proportion_nodes_used     = proportion_nodes_used,
    proportion_big_components = proportion_big_components,
    degree_power              = degree_power,
    degree_fit_r2             = degree_summary$r.squared,
    degree_mean               = mean(degree),
    degree_median             = median(degree),
    degree_max                = max(degree)
  )

}
