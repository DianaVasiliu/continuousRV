csum <- function(fx, fy) {
  function(z) {
    integral(function(x) {
      fx(z - x) * fy(x)
    }, -Inf, Inf)
  }
}
