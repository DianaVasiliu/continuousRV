imoment <- function(pdf, ord) {
  ev_finder <- function(transform = identity) {
      tryCatch (
        { integrate(function(x) {transform(x) * pdf(x)}, -Inf, Inf)$value },
        error = function(e) {
          message(paste("Couldn't find initial moment of given order ", ord))
        }
      )
  }

  ev_finder(function(x) x ^ ord)
}
