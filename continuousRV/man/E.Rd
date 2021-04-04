\name{E}
\alias{E}

\title{
    Expected value of a continuous random variable
}
\description{
    The function calculates the expected value \code{E} of a continuous random variable. The expected value is calculated using the \code{PDF} function given as argument.

}

\usage{
E(pdf, ...)
}

\arguments{
  \item{pdf}{
  the probability density function linked to the random variable
  }
  \item{...}{
  further arguments passed to or from other methods
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
    The function returns a floating point number, representing the expected value of the random variable.\cr
}

\references{
\link{https://mikmart.rbind.io/2018/02/17/finding-expected-values-of-random-variables/}
}
\author{
Ioana Grigore
}

\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
# test 1:

E(dexp)
E(dnorm)
E(dunif)
E(dnorm, mean=9)


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

}
