Cov <- function(fcommon, interval_x, interval_y) {

    lowx <- head(interval_x, 1)
    highx <- tail(interval_x, 1)
    lowy <- head(interval_y, 1)
    highy <- tail(interval_y, 1)

    lista <- marginalPDF(fcommon, interval_x, interval_y)
    fx <- lista[[1]]
    fy <- lista[[2]]

    muX <- E(fx)
    muY <- E(fy)

    I <- integrate(function(y) {
        sapply(y, function(y) {
            integrate(function(x) {
                x*y*fcommon(x,y)
            }, lowx, highx)$value
        })
    }, lowy, highy)$value

    I - muX * muY
}

