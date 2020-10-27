#' Generate a sequence of threshold values given a 'dist' object
#'
#' @param x an object of class `dist`
#' @param sort_decreasing should the resulting table be sorted in descending order
#' @param length.out the number of threshold values to return
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
#' @export
evaluate_threshold <- function(x, thresh) {

  g <- graph_from_adjacency_matrix(
    adjmatrix = signum_adjacency(as.matrix(x), thresh, "less"),
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
  prop_big_component <- max_component / n

  degree <- igraph::degree(g)
  degree_table <- table(degree)
  degree_df <- as.data.frame(degree_table)
  degree_df$degree <- as.numeric(as.character(degree_df$degree))
  degree_fit <- lm(log2(Freq) ~ log2(degree), data = degree_df, subset = -1)
  degree_summary <- summary(degree_fit)
  degree_power <- -unname(coef(degree_fit)[2])

  list(
    threshold     = thresh,
    degree_power  = degree_power,
    degree_fit_r2 = degree_summary$r.squared,
    degree_mean   = mean(degree),
    degree_median = median(degree),
    degree_max    = max(degree),
    diameter      = D,
    mean_path_length         = mean_pl,
    mean_cluster_coefficient = mean_cc
  )

}
