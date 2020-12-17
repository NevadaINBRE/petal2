read_sample_input <- function(file) {
  f <- read.delim(file)
  f <- as.matrix(f)
  f <- t(f)
  f
}

SampleData <- read_sample_input("data-raw/SampleInputFile1.txt")

usethis::use_data(SampleData)
