\name{csum}
\alias{csum}

\title{
    Sum and difference of two continuous r.v.
}

\description{
    The functions calculate the sum and the difference of two continuous r.v.
}

\usage{
    csum(fx, fy)
    cdif(fx, fy)
}

\arguments{
  \item{fx}{
  the \code{PDF} of the random variable X
  }

  \item{fy}{
  tte \code{PDF} of the random variable Y
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
The function returns another function which represents the \code{PDF} of the random variable resulted from either X + Y or X - Y.
}

\references{
%% ~put references to the literature/web site here ~
}

\author{
Matei-Stefan Cibotaru
}

\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# For csum():
plot(Vectorize(csum(function(x) {
                        dnorm(x, mean = 5)
                    },
                    function(x) {
                        dnorm(x, mean = -2)
                    })
               ), from=-10, to=10)


# For cdif():
plot(Vectorize(cdif(function(x) {
                        dnorm(x, mean = 5)
                    },
                    function(x) {
                        dbeta(x, 1, 3)
                    })
               ), from=-10, to=10)
}
