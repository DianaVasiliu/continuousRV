cmoment <- function(pdf, ord, mean=0) {
  ev_finder <- function(transform = identity) {
      tryCatch (
        { integrate(function(x) {transform(x) * pdf(x)}, -Inf, Inf)$value },
        error = function(e) {
          message(paste("Couldn't find centered moment of given order", ord))
        }
      )
  }

  ev_finder(function(x) (x - mean) ^ ord)
}
