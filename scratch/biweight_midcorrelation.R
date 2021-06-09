mad <- function(x) median(abs(x - median(x)))
uv <- function(x) (x - median(x)) / (9*mad(x))
wI <- function(x) x > 0
w <- function(u) (1 - u^2)^2 * wI(1 - abs(u))

tilde <- function(x) {
  xm <- x - median(x)
  ws <- w(uv(x))
  xi_wi <- xm * ws
  den <- sqrt(sum((xi_wi)^2))
  return(xi_wi / den)
}

bicor <- function(x, y) {
  sum(tilde(x) * tilde(y))
}

n <- 100000
x <- rnorm(n)
y <- rnorm(n, 3*x+2, 3)

X <- matrix(rnorm(100*1000), 100, 1000)

cor(x, y)
WGCNA::bicor(x, y)
bicor(x, y)

system.time({
  bicor(x, y)
})

system.time({
  WGCNA::bicor(x, y)
})

system.time({
  WGCNA::bicor(X, nThreads = 12)
})
