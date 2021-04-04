\name{Cov}
\alias{Cov}

\title{
    Covariance of two continuous random variables
}

\description{
    The function calculates the covariance of two continuous random variables.
}

\usage{
Cov(fcommon, interval_x, interval_y)
}

\arguments{
  \item{fcommon}{
  the common probability density function (\code{PDF}) of the random variables
  }

  \item{interval_x}{
  the domain of the random variable X
  }

  \item{interval_y}{
  the domain of the random variable Y
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
The function returns a real value representing the covariance of the two random variables of which common \code{PDF} is given as argument.
}

\references{
\link{https://stackoverflow.com/questions/8913603/calculating-double-integrals-in-r-quickly}
}

\author{
Diana-Elena Vasiliu
}

\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
# the common PDF: (X,Y) : [0,1]x[0,1], f(x,y) = x + y

f1 <- function(x, y) {
    if (isTRUE(x >= 0 && x <= 1 && y >= 0 && y <= 1))
      x + y
    else
      0
}

tx <- seq(0,1,0.001)
ty <- seq(0,1,0.001)

Cov(f1, tx, ty)

}
