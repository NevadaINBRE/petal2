petal_methods <- c(
  "pearson",
  "spearman",
  "kendall"
  # "petal_euclidean"
)

parallelDist_methods <- c(
  # User defined
  "custom",

  # Distance methods for continuous input variables
  "bhjattacharyya",
  "bray",
  "canberra",
  "chord",
  "divergence",
  "euclidean",
  "fJaccard",
  "geodesic",
  "hellinger",
  "kullback",
  "mahalanobis",
  "manhattan",
  "maximum",
  "minkowski",
  "podani",
  "soergel",
  "wave",
  "whittaker",

  #Distance methods for binary input variables
  "binary",
  "braun-blanquet",
  "cosine",
  "dice",
  "fager",
  "faith",
  "hamman",
  "hamming",
  "kulczynski1",
  "kulczynski2",
  "michael",
  "mountford",
  "mozley",
  "ochiai",
  "phi",
  "russel",
  "simple matching",
  "simpson",
  "stiles",
  "tanimoto",
  "yule",
  "yule2"
)

all_methods <- c(petal_methods, parallelDist_methods)

dist_methods <- list(
  petal = petal_methods,
  parallelDist = parallelDist_methods,
  all = all_methods
)

usethis::use_data(dist_methods, overwrite = TRUE, internal = TRUE)
