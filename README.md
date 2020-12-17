
<!-- README.md is generated from README.Rmd. Please edit that file -->

# petal2

<!-- badges: start -->

<!-- badges: end -->

Create small-world, scale-free networks from genomic data sets.

## Installation

<!-- You can install the released version of petal2 from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("petal2") -->

<!-- ``` -->

You can install the development version of petal2 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NevadaINBRE/petal2")
```

## Example Use

This is a basic example which shows you how to construct a small-world,
scale-free network:

``` r
library(petal2)
dat <- SampleData # in the petal2 package

# Sample data has missing values
dat <- dat[, complete.cases(t(dat))]

# Create a metric matrix, in this case pairwise bi-weight mid-correlation
mm <- metric_matrix(dat, method = "bicor")
#> 

# Find a threshold that produces a small-world, scale-free network
ret <- bayes_thresh(
  mm = mm, 
  bounds = c(0.5, 0.99),
  init_points = 2,
  n_iter = 8,
  verbose = TRUE
)
#> elapsed = 5.58   Round = 1   x = 0.6027  Value = -2.0102 
#> elapsed = 7.43   Round = 2   x = 0.5177  Value = -2.0007
#> Warning in GPfit::GP_fit(X = Par_Mat[Rounds_Unique, ], Y =
#> Value_Vec[Rounds_Unique], : X should be in range (0, 1)
#> elapsed = 0.57   Round = 3   x = 0.9402  Value = -20.0946 
#> elapsed = 3.33   Round = 4   x = 0.6945  Value = -2.2525 
#> elapsed = 7.78   Round = 5   x = 0.5000  Value = -2.0004 
#> elapsed = 3.87   Round = 6   x = 0.6513  Value = -2.0532 
#> elapsed = 6.36   Round = 7   x = 0.5536  Value = -2.0021 
#> elapsed = 7.01   Round = 8   x = 0.5254  Value = -2.0009 
#> elapsed = 7.97   Round = 9   x = 0.5000  Value = -2.0004 
#> elapsed = 7.22   Round = 10  x = 0.5294  Value = -2.0010 
#> 
#>  Best Parameters Found: 
#> Round = 5    x = 0.5000  Value = -2.0004

# Create an adjacency matrix based on the winning threshold
am <- signum_adjacency(mm, tau = ret$Best_Par, method = "less")

# Create an igraph object from the adjacency matrix
g <- igraph::graph_from_adjacency_matrix(am, "undirected", diag = FALSE)
```

## Common gotchas

  - The input data frame or matrix must be organized such that the rows
    represent observations (conditions) and the columns represent the
    variables (gene expressions)
