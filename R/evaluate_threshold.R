#' Evaluate the properties of a graph determined by a threshold value
#'
#'
#'
#' @param x an object of class `dist`
#' @param thresh a cutoff value to create the adjacency matrix
#' @param method which side of the cutoff value to set to 0
#' @importFrom stats coef lm median
#' @export
evaluate_threshold <- function(x, thresh, method = "less") {

  # Create the adjacency matrix and remove any unconnected nodes.
  # Need to save the number of nodes before and after filtering in order to
  # calculate the proportion of nodes used.
  # TODO: See if igraph can use sparse matrices for graph creation
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


  g_diameter <- igraph::diameter(g)
  mean_cc <- igraph::transitivity(g, type = "average")  # cluster coefficient
  mean_pl <- igraph::mean_distance(g, directed = FALSE) # average path length

  g_components  <- igraph::components(g)    # AKA "clusters"
  n_components  <- g_components$no
  max_component <- max(g_components$csize)

  proportion_nodes_used <- N1 / N0
  proportion_big_components <- max_component / N1

  # The number (Freq) of nodes of degree (degree) follows a power law distribution.
  # We need the power, a, in F(k) ~ C*k^(-a)
  degree           <- igraph::degree(g)
  degree_table     <- table(degree)
  degree_df        <- as.data.frame(degree_table)
  # The data frame turns the degree labels into factors. We need the labels as
  # numeric data, so we need to convert the factors to characters first, otherwise
  # as.numeric(factor_variable) returns the integer representation of the levels
  degree_df$degree <- as.numeric(as.character(degree_df$degree))
  degree_fit       <- lm(log2(Freq) ~ log2(degree), data = degree_df)
  degree_fit_r2    <- summary(degree_fit)$r.squared
  degree_power     <- -unname(coef(degree_fit)[2])

  ret <- c(
    threshold                 = thresh,
    diameter                  = g_diameter,
    mean_path_length          = mean_pl,
    mean_cluster_coefficient  = mean_cc,
    number_of_components      = n_components,
    proportion_nodes_used     = proportion_nodes_used,
    proportion_big_components = proportion_big_components,
    degree_power              = degree_power,
    degree_fit_r2             = degree_fit_r2,
    degree_mean               = mean(degree),
    degree_median             = median(degree),
    degree_max                = max(degree)
  )
  loss <- thresh_loss(as.list(ret))
  c(ret, loss = loss)
}
