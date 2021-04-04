E <- function(pdf, ...) {
  # formula de integrare pentru medie:
  # E(X) = integrala( x*f(x), -inf, inf)
  f <- function(x) {
      x * pdf(x, ...)
  }
  f <- Vectorize(f)
  I <- integrate(f, -Inf, Inf)
  I$value
}
