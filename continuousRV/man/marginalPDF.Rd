\name{marginalPDF}
\alias{marginalPDF}

\title{
    Marginal probability density functions
}

\description{
    The function returns a list of two elements representing the marginal distribution for X at position \code{[[1]]} and for Y at position \code{[[2]]}.\cr\cr
    The two elements returned are functions of x and respectively y.
}

\usage{
marginalPDF(fcommon, interval_x, interval_y)
}

\arguments{
  \item{fcommon}{
  the common probability density function of the two continuous random variables
  }

  \item{interval_x}{
  the domain of X
  }

  \item{interval_y}{
  the domain of Y
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
    The function returns a list of two elements representing the two marginal densities.\cr\cr
    To get the values from the list, it must be indexed by either \code{[[1]]} or \code{[[2]]}.
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
# Given the following common PDF: (X,Y) : [0,1]x[0,1], f(x,y) = x + y
f1 <- function(x, y) {
  if (isTRUE(x >= 0 && x <= 1 && y >= 0 && y <= 1))
    x + y
  else
    0
}

tx <- seq(0,1,0.001)
ty <- seq(0,1,0.001)

# Fiding the marginal densities indexed properly
fx <- marginalPDF(f1, tx, ty)[[1]]
fy <- marginalPDF(f1, tx, ty)[[2]]

# Checking if the resulted PDFs are indeed PDFs
isPDF(fx, tx)   # returns TRUE
isPDF(fy, ty)   # returns TRUE
}
