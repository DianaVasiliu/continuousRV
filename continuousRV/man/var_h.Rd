\name{var_h}
\alias{var_h}

\title{
    The variation of a function of a r.v.
}

\description{
    The function calculates the variation of a random variable \code{h(X)}, where \code{X} is a continuous random variable and \code{h} is a continuous function.
}

\usage{
var_h(h, pdf)
}

\arguments{
  \item{h}{
  the function that is applied on the random variable X
  }

  \item{pdf}{
  the probability density function of the random variable X
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
The function returns a real number representing the variation of \code{h(X)}.
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

}
