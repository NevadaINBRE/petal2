new_metric_matrix <- function(x, metric) {
  is_symmetric <- attr(metric, "is_symmetric")
  d <- dim(x)

  if (is_symmetric) {
    # Create lower triangular Matrix::sparseMatrix()
  } else {
    # Create dense matrix
  }
}
