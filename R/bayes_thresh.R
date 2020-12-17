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
