generate <- function() {
    number <- readline(prompt="Enter the number of values desired: ")
    # convert character into integer
    number <- as.integer(number)


    repartitionType <- readline(prompt="Enter the type of repartition to be used: ")

    #work on interval[a, b]
    a <- 4
    b <- 10

    sd <- 10
    maxCreate <- 2000

    if(repartitionType == "rnorm")
    {
      info <- data.frame(draw = rnorm(maxCreate, 0, sd))
    }

    if(repartitionType == "runif")
    {
      info <- data.frame(draw = runif(number, a, b))
    }

    if(repartitionType == "rexp")
    {
      info <- data.frame(draw = rexp(number, 1/7))
    }

    f <- sample(info$draw[!with(info, draw > b | draw < a)], number, replace = F)

    text <- paste("We have produced", number, "numbers:")
    print(text)
    print(f)
    hist(f, main = repartitionType, freq = F)
}
