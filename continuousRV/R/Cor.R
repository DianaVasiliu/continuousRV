Cor <- function(fcommon, interval_x, interval_y) {

    lowx <- head(interval_x, 1)
    highx <- tail(interval_x, 1)
    lowy <- head(interval_y, 1)
    highy <- tail(interval_y, 1)

    lista <- marginalPDF(fcommon, interval_x, interval_y)
    fx <- lista[[1]]
    fy <- lista[[2]]

    covXY <- Cov(fcommon, interval_x, interval_y)
    varX <- Var(fx)
    varY <- Var(fy)

    covXY / sqrt(varX * varY)
}
