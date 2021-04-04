\name{isPDF}
\alias{isPDF}

\title{
    PDF function checker
}

\description{
    The function determines whether a given function can be a probability density function (\code{pdf}) or not.\cr\cr
    A \code{pdf f} must be greater or equal to 0 on its entire domain and the improper integral \code{integrate(f, -Inf, Inf)} must be equal to 1. If any of these condition is skipped, then the function will generate an error message and return \code{NULL}.

}

\usage{
isPDF(f, domain = NULL)
}

\arguments{
  \item{f}{
      the function that needs to be checked whether it can be a \code{pdf} or not
  }

  \item{domain}{
      the domain of the function given as parameter
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
    The function returns \code{TRUE} or \code{FALSE}, depending on compliance with the conditions of a \code{pdf}.\cr\cr
    If a domain is not provided as parameter, the function is undefined on the given domain or the value of the integral cannot be calculated, the function will provide an error message and will return \code{NULL}.
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
# test 1
ans <- isPDF(function(x) {
  return(1/x)
},
seq(0.001, 2, 0.001))

# test 2
ans <- isPDF(function(x) {
  ifelse(x < 0 || x > 1/3, 0, 3)
},
seq(-1, 1, 0.001))

# test 3
f1 <- function(x) {
  return(x + 2)
}

ans <- isPDF(f1)

# test 4
ans <- isPDF(function(x) {
  return(1/x)
},
seq(0, 2, 0.001))

# test 5
ans <- isPDF(function(x) {
  return(x + 2)
},
seq(0, 2, 0.001))
}
