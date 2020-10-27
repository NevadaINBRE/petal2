#' @export
signum_adjacency <- function(x, tau, method = c("less", "greater")) {
    method <- match.arg(method)
    op <- ifelse(method == "less", `<`, `>`)
    ifelse(op(x, tau), 0L, 1L)
}
