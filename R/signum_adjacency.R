#' Filter a matrix by a simple threshold value
#'
#' @param x a numeric vector or matrix
#' @param tau a cutoff value
#' @param method which side of the cutoff value to set to 0
#' @export
signum_adjacency <- function(x, tau,
                             method = c("less", "le", "ge", "greater")) {
    method <- match.arg(method)

    op <- switch (method,
      "less"    = `<`,
      "le"      = `<=`,
      "ge"      = `>=`,
      "greater" = `>`
    )

    b <- op(x, tau)

    x[b] <- 0
    x[!b] <- 1
    x
}
