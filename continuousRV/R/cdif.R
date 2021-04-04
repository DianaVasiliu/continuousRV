cdif <- function(fx, fy) {
  function(z) {
    integral(function(x) {
      fx(x - z) * fy(x)
    }, -Inf, Inf)
  }
}
