marginalPDF <- function(fcommon, interval_x, interval_y) {

  lowx <- head(interval_x, 1)
  highx <- tail(interval_x, 1)
  lowy <- head(interval_y, 1)
  highy <- tail(interval_y, 1)

  fx <- function(x) {
    sapply(x, function(x) {
      integrate(Vectorize(function(y) {
        fcommon(x,y)
      }), lowy, highy)$value
    })
  }

  fy <- function(y) {
    sapply(y, function(y) {
      integrate(Vectorize(function(x) {
        fcommon(x,y)
      }), lowx, highx)$value
    })
  }

  lista <- c(fx, fy)

  return(lista)

}
