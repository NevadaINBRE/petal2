#' Filter a matrix by a simple threshold value
#'
#' @param x a numeric vector or matrix
#' @param tau a cutoff value
#' @param method which side of the cutoff value to set to 0
#' @export
signum_adjacency <- function(x, tau, method = c("less", "greater")) {
    method <- match.arg(method)
    op <- ifelse(method == "less", `<`, `>`)
    ifelse(op(x, tau), 0L, 1L)
}
