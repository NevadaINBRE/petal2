new_metric_matrix <- function(x, metric, labels) {
  structure(x, class = "MetricMatrix",
            metric = metric,
            labels = labels)
}




validate_metric_matrix <- function(mm) {
  x <- unclass(mm)
  m <- attr(mm, "metric")
  l <- labels(mm)
  # Properties
  # 1) Square
  # 2) Homogeneous diagonal
  # 3) length(l) == nrow(x)
  mm
}




#' Create a metric matrix from a matrix of -omics data
#'
#' @param x A matrix with the conditions in the rows and the -omics data in the columns
#' @param metric An object of class `Metric`
#' @param labels the labels or names of the columns (-omics)
#' @return An object of class `MetricMatrix`
#' @export
MetricMatrix <- function(x, metric, labels = colnames(x)) {
  d <- ncol(x)
  if (is.null(labels)) labels <- paste0("V", 1:d)

  if (is_symmetric(metric)) {
    # Do symmetric distance
    ij <- utils::combn(d, 2) # get lower triangular coordinates
    mm <- numeric(length = choose(d, 2))
    for (idx in 1:choose(d,2)) {
      mm[idx] <- metric(x[,ij[1,idx]],
                        x[,ij[2,idx]])
    }
  } else {
    # Do dense distance
    mm <- matrix(NA_real_, d, d)
    for (i in 1:d) {
      xj <- x[,j]
      for (j in 1:d) {
        mm[i,j] <- metric(x[,i], xj)
      }
    }
  }

  validate_metric_matrix(new_metric_matrix(mm, metric, labels))
}
