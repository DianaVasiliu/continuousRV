\name{Var}
\alias{Var}

\title{
    Variance of a continuous random variable
}

\description{
    The function calculates the variance (\code{Var}) of a continuous random variable.
}

\usage{
Var(pdf, ...)
}

\arguments{
  \item{pdf}{
  the probability density function of the continuous random variable
  }
  \item{...}{
  further arguments passed to or from other methods
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
The function returns a number representing the variance of the random variable which has the \code{PDF} given as parameter.
}

\references{
\link{
https://mikmart.rbind.io/2018/02/17/finding-expected-values-of-random-variables/}
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
}
