valc <- function(f) {
  if(isPDF(f, seq(-100000, 100000, 0.01))) {
    r <- function(x) {
      integral(f, -Inf, x)
    }
    ri <- inverse(r)
    init <- runif(100)
    res <- c()
    for(i in init) {
      res = c(res, ri(i))
    }
    res
  } else {
    "Error: Please use a probability density function"
  }
}
