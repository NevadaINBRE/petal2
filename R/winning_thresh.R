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


#' Compute the winning threshold using Bayesian Optimization
#'
#' This is a simple wrapper for `rBayesianOptimization::BayesianOptimization`
#'   that seeks to discover a threshold that produces a small-world, scale-free
#'   network over a given range. Generally 10 or more iterations (including
#'   init points) is needed to find an optimal value. For smaller networks (
#'   under 10000 genes), it should be no problem to run 20 or more iterations
#'   within a few minutes.
#'
#' @param mm A metric matrix or an object of class 'dist'
#' @param bounds A numeric vector of lower and upper bounds. All the sample
#'   points in `init_grid_dt` should be in the range of bounds.
#' @param init_grid_dt A numeric vector of user specified points to sample the
#'   target function.
#' @param init_points Number of randomly chosen points to sample the target
#'   function before Bayesian Optimization fitting the Gaussian Process.
#' @param n_iter Total number of times the Bayesian Optimization is to repeated.
#' @param acq Acquisition function type to be used. Can be "ucb", "ei" or "poi".
#' \itemize{
#'   \item \code{ucb} GP Upper Confidence Bound
#'   \item \code{ei} Expected Improvement
#'   \item \code{poi} Probability of Improvement
#' }
#' @param kappa tunable parameter kappa of GP Upper Confidence Bound, to balance exploitation against exploration,
#'   increasing kappa will make the optimized hyperparameters pursuing exploration.
#' @param eps tunable parameter epsilon of Expected Improvement and Probability of Improvement, to balance exploitation against exploration,
#'   increasing epsilon will make the optimized hyperparameters are more spread out across the whole range.
#' @param kernel Kernel (aka correlation function) for the underlying Gaussian Process. This parameter should be a list
#'   that specifies the type of correlation function along with the smoothness parameter. Popular choices are square exponential (default) or matern 5/2
#' @param verbose Whether or not to print progress.
#' @param ... Other arguments passed on to \code{\link[GPfit]{GP_fit}}.
#' @return a list of Bayesian Optimization result is returned:
#' \itemize{
#'   \item \code{Best_Par} a named vector of the best hyperparameter set found
#'   \item \code{Best_Value} the value of metrics achieved by the best hyperparameter set
#'   \item \code{History} a \code{data.table} of the bayesian optimization history
#'   \item \code{Pred} a \code{data.table} with validation/cross-validation prediction for each round of bayesian optimization history
#' }
#'
#' @importFrom rBayesianOptimization BayesianOptimization
#' @export
bayes_thresh <- function(mm, bounds,
                         init_grid_dt = NULL,
                         init_points = 0,
                         n_iter,
                         acq = "ucb",
                         kappa = 2.576,
                         eps = 0,
                         kernel = list(type = "exponential", power = 2),
                         verbose = FALSE,
                         ...) {
  f <- function(x) {
    list(Score = -1 * thresh_loss(as.data.frame(evaluate_threshold(mm, x))),
         Pred  = 0.0)
  }

  BayesianOptimization(
    FUN = f,
    bounds = list(x = bounds),
    init_grid_dt = init_grid_dt,
    init_points = init_points,
    n_iter = n_iter,
    acq = "ucb",
    kappa = kappa,
    eps = eps,
    kernel = kernel,
    verbose = verbose,
    ...
  )
}
