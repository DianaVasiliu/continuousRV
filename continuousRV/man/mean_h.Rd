\name{mean_h}
\alias{mean_h}

\title{
    Expected value of a function of a random variable
}

\description{
    The function implements the classic formulas for the expected value of a random variable, but having in mind that it is a function of X, not X itself.
}

\usage{
mean_h(h, pdf)
}

\arguments{
  \item{h}{
  the function that is applied on the continuous random variable X (\code{h(X)})
  }
  \item{pdf}{
  the probability density function of the random variable X
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
The function returns a real number representing the mean (expected value) of the random variable \code{h(X)}, where \code{X} is a continuous random variable and \code{h} is a continuous function.
}

\references{
%% ~put references to the literature/web site here ~
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
}
