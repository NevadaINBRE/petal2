#' Pick the winning threshold the produces a small-world/scale-free network
#'
#' @export
winning_thresh <- function(
  thresh_df,
  min_r2 = 0.8,
  min_power = 1,
  min_prop_used = 0.40,
  min_prop_big_comp = 0.95) {

  candidates <- dplyr::filter(thresh_df,
      degree_fit_r2 >= min_r2,
      degree_power >= min_power,
      proportion_nodes_used >= min_prop_used,
      proportion_big_components >= min_prop_big_comp
    )

  n_candidates <- nrow(candidates)

  if (n_candidates > 1) {
    # degree fit r2    - higher is better
    # degree power     - higher is better
    # mean cC          - higher is better
    # mean path length - lower is better
    # prop used        - higher is better
    # prop big comp    - higher is better
    winner <- dplyr::mutate(candidates,
      rank_r2    = rank(-degree_fit_r2,             ties.method = "min"),
      rank_power = rank(-degree_power,              ties.method = "min"),
      rank_cc    = rank(-mean_cluster_coefficient,  ties.method = "min"),
      rank_path  = rank(mean_path_length,           ties.method = "min"),
      rank_prop  = rank(-proportion_nodes_used,     ties.method = "min"),
      rank_big   = rank(-proportion_big_components, ties.method = "min"),
      sum_inv_rank = sum(1 / rank_r2:rank_big)
    )
    winner <- dplyr::arrange(winner, sum_inv_rank)
    winner <- dplyr::select(-c(rank_r2:sum_inv_rank))
    winner <- head(winner, 1)
  } else if (n_candidates == 1) {
    winner <- candidates
  } else {
    warning("None of the considered thresholds produce a small-world/scale-free network")
    winner <- NULL
  }

  winner

}
