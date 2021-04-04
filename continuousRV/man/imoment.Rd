\name{imoment}
\alias{imoment}

\title{
    Initial moment of order r
}

\description{
    The function calculates the initial moment of order \code{r} of a continuous random variable.
}

\usage{
imoment(pdf, ord)
}

\arguments{
  \item{pdf}{
  the probability density function of the random variable
  }
  \item{ord}{
  the order that needs to be calculated
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
    The function returns a value which represents the initial moment of given order.\cr\cr
    If the moment does not exist, a message will be displayed for the user.
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
}
