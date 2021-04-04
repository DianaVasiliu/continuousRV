\name{normConst}
\alias{normConst}

\title{
    Normalization constant
}

\description{
    The function finds the normalization constant of a function given as argument.\cr\cr
    If there is no domain provided, the function will stop and return \code{NULL}.
}

\usage{
normConst(f, domain = NULL)
}

\arguments{
  \item{f}{
    the function that needs to be normalized
  }

  \item{domain}{
    the domain of the function
  }
}

\details{
    The domain must be not \code{NULL}. If no domain is provided, then the \code{normConst} function will generate a domain error and return \code{NULL}.\cr\cr
    If the normalization constant cannot be determined or is unexistent, the \code{normConst} function will generate a value error and return \code{NULL}.
}

\value{
    The function returns a number representing the normalization constant for the function given as argument or \code{NULL} if the constant can't be determined.
}

\references{
%% ~put references to the literature/web site here ~
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
f1 <- function(x) {
  1/x
}

domain <- seq(0.001, 1, 0.001)

x <- normConst(f1, domain)
}

