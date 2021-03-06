% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_matrix.R
\name{metric_matrix}
\alias{metric_matrix}
\title{Create a metric matrix}
\usage{
metric_matrix(x, method, ...)
}
\arguments{
\item{x}{a numeric matrix where each row is an observation/condition and
each column is a variable/gene}

\item{method}{the distance measure to be used. A list of all available
distance methods can be found in the details section below and in
\code{\link[parallelDist]{parDist}}}

\item{...}{additional parameters which will be passed to the distance methods}
}
\value{
an object of class 'dist'
}
\description{
This is a wrapper for \code{\link[parallelDist]{parDist}} with a few default
settings to be specialized for the petal2 package. Namely the diagonal is
never stored and the lower triangular part of the distance matrix is used.
}
