#' Pick the winning threshold the produces a small-world/scale-free network
#'
#' @param thresh_df The input threshold data frame
#' @param min_r2 Minimum r-squared for the fit
#' @param min_power Minimum degree of the power for the fit
#' @param min_prop_used Minimum proportion of nodes used
#' @param min_prop_big_comp Minimum proportion of big components
#' @export
winning_thresh <- function(
  thresh_df,
  min_r2 = 0.8,
  min_power = 1,
  min_prop_used = 0.40,
  min_prop_big_comp = 0.95) {


  candidates <- subset(thresh_df,
    thresh_df$degree_fit_r2             >= min_r2 &
    thresh_df$degree_power              >= min_power &
    thresh_df$proportion_nodes_used     >= min_prop_used &
    thresh_df$proportion_big_components >= min_prop_big_comp)

  n_candidates <- nrow(candidates)

  if (n_candidates > 1) {
    # degree fit r2    - higher is better
    # degree power     - higher is better
    # mean CC          - higher is better
    # mean path length - lower is better
    # prop used        - higher is better
    # prop big comp    - higher is better
    rank_df <- list()
    rank_df$rank_r2    <- rank(-candidates$degree_fit_r2, ties.method = "min")
    rank_df$rank_power <- rank(-candidates$degree_power, ties.method = "min")
    rank_df$rank_cc    <- rank(-candidates$mean_cluster_coefficient, ties.method = "min")
    rank_df$rank_path  <- rank( candidates$mean_path_length, ties.method = "min")
    rank_df$rank_prop  <- rank(-candidates$proportion_nodes_used, ties.method = "min")
    rank_df$rank_big   <- rank(-candidates$proportion_big_components, ties.method = "min")

    # Turn the ranks into inverse ranks and compute the row sums
    rank_df <- lapply(rank_df, function(x) 1 / x)
    rank_df <- as.data.frame(rank_df)
    sum_inv_rank <- rowSums(rank_df)

    winning_idx <- which.max(sum_inv_rank)
    winner <- candidates[winning_idx, ]
  } else if (n_candidates == 1) {
    winner <- candidates
  } else {
    warning(paste(
      "None of the considered thresholds produce a small-world/scale-free",
      "network. Defaulting to the closest candidate."))
    winner <- thresh_df[which.min(thresh_loss(thresh_df)), ]
  }

  winner

}


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
