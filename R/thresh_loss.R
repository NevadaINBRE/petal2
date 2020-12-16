#' Calculate the loss function for a data frame of thresholds
#'
#' @param thresh_df The input threshold data frame
#' @export
thresh_loss <- function(thresh_df) {

  # scale-free properties
  x1 <- 1 / (1 + exp((thresh_df$degree_fit_r2 - 0.75) / 0.0025))
  x2 <- 1 / (1 + exp((thresh_df$degree_power - 0.99) / 0.01))

  # small-world properties
  x3 <- 10 / (1 + exp((thresh_df$mean_cluster_coefficient - 0.39) / 0.01))
  x4 <- 10 / (1 + exp(-(thresh_df$mean_path_length - 9.25) / 0.1))

  # not so important
  x5 <- (thresh_df$proportion_nodes_used - 1.0)^2

  # Extremely important
  x6 <- 10 / (1 + exp((thresh_df$proportion_big_components - 0.89) / 0.001))

  x1 + x2 + x3 + x4 + x5 + x6
}
