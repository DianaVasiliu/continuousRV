#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     1)           normConst()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

f1 <- function(x) {
  1/x
}

domain <- seq(0.001, 1, 0.001)

x <- normConst(f1, domain)
x

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     2)           isPDF()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# test 1 - FALSE
ans <- isPDF(function(x) {
  return(1/x)
},
seq(0.001, 2, 0.001))

ans


# test 2 - TRUE
ans <- isPDF(function(x) {
  ifelse(x < 0 || x > 1/3, 0, 3)
},
seq(-1, 1, 0.001))

ans


# test 3 - Domain error
f1 <- function(x) {
  return(x + 2)
}

ans <- isPDF(f1)

ans


# test 4 - Undefined function error
ans <- isPDF(function(x) {
  return(1/x)
},
seq(0, 2, 0.001))

ans


# test 5 - Divergent integral error
ans <- isPDF(function(x) {
  return(x + 2)
},
seq(0, 2, 0.001))

ans


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     3)           valc()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

valc(dnorm)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     4)           plots()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plots(seq(-2,5,0.001), exp=T, cdf=T)
plots(seq(-2,5,0.001))
plots(seq(-2,5,0.001), norm=T, exp=T, cdf=T, pdf=T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     5.1)           E()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# test 1:

E(dexp)
E(dnorm)
E(dunif)


# test 2:
# Given the PDF function f:

f <- function(x) {
  g <- c()
  for (i in x) {
    if (isTRUE(0 < i) && isTRUE(i < 3)) {
      g <- c(g, (1/9)*(i^2))
    }
    else {
      g <- c(g, 0)
    }
  }
  g
}

isPDF(f, seq(0,4,0.001))

plot(seq(-4,4,0.001), f(seq(-4,4,0.001)))

E(f)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     5.2)           var()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# test 1:
Var(dnorm, mean=6)


# test 2:
# Given the PDF function f:

f <- function(x) {
  g <- c()
  for (i in x) {
    if (isTRUE(0 < i) && isTRUE(i < 3)) {
      g <- c(g, (1/9)*(i^2))
    }
    else {
      g <- c(g, 0)
    }
  }
  g
}

Var(f)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     5.3)           imoment()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# test 1:
imoment(dnorm, 4)


# test 2:
# Given the PDF function f:

f <- function(x) {
  g <- c()
  for (i in x) {
    if (isTRUE(0 < i) && isTRUE(i < 3)) {
      g <- c(g, (1/9)*(i^2))
    }
    else {
      g <- c(g, 0)
    }
  }
  g
}

imoment(f, 4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     5.4)           cmoment()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# test 1:
cmoment(dnorm, 4, E(dnorm))


# test 2:
# Given the PDF function f:

f <- function(x) {
  g <- c()
  for (i in x) {
    if (isTRUE(0 < i) && isTRUE(i < 3)) {
      g <- c(g, (1/9)*(i^2))
    }
    else {
      g <- c(g, 0)
    }
  }
  g
}

cmoment(f, 4, E(f))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       6.1)          mean_h()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Given the following PDF:
f <- function(x) {
  g <- c()
  for (i in x) {
    if (isTRUE(0 < i) && isTRUE(i < 3)) {
      g <- c(g, (1/9)*(i^2))
    }
    else {
      g <- c(g, 0)
    }
  }
  g
}

# Function h(X) = X^2
h <- function(x) {
  if (isTRUE(x >= 0))
    x^2
  else
    0
}

mean_h(h,f)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       6.2)          var_h()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Given the PDF:
f <- function(x) {
  g <- c()
  for (i in x) {
    if (isTRUE(0 < i) && isTRUE(i < 3)) {
      g <- c(g, (1/9)*(i^2))
    }
    else {
      g <- c(g, 0)
    }
  }
  g
}

# Function h(X) = X^2
h <- function(x) {
  if (isTRUE(x >= 0))
    x^2
  else
    0
}

var_h(h,f)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       9)           generate()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The function does all the work:
generate()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     10.1)           Cov()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# the common distribution: (X,Y) : [0,1]x[0,1], f(x,y) = x + y

f1 <- function(x, y) {
  if (isTRUE(x >= 0 && x <= 1 && y >= 0 && y <= 1))
    x + y
  else
    0
}

tx <- seq(0,1,0.001)
ty <- seq(0,1,0.001)

Cov(f1, tx, ty)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     10.2)           Cor()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# the common distribution: (X,Y) : [0,1]x[0,1], f(x,y) = x + y

f1 <- function(x, y) {
  if (isTRUE(x >= 0 && x <= 1 && y >= 0 && y <= 1))
    x + y
  else
    0
}

tx <- seq(0,1,0.001)
ty <- seq(0,1,0.001)

Cor(f1, tx, ty)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     11)           marginalPDF()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f1 <- function(x, y) {
  if (isTRUE(x >= 0 && x <= 1 && y >= 0 && y <= 1))
    x + y
  else
    0
}

tx <- seq(0,1,0.001)
ty <- seq(0,1,0.001)

fx <- marginalPDF(f1, tx, ty)[[1]]
fy <- marginalPDF(f1, tx, ty)[[2]]

isPDF(fx, tx)
isPDF(fy, ty)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     12.1)           csum()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot(Vectorize(csum(function(x) {
                        dnorm(x, mean = 5)
                    },
                    function(x) {
                        dnorm(x, mean = -2)
                    })
               ), from=-10, to=10)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     12.2)           cdif()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot(Vectorize(cdif(function(x) {
                        dnorm(x, mean = 5)
                    },
                    function(x) {
                        dbeta(x, 1, 3)
                    })
               ), from=-10, to=10)

