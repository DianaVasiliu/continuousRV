\name{plots}
\alias{plots}

\title{
  Plotting some examples of basic repartitions
}
\description{
  The function plots some examples of basic repartitions, which are Normal distribution, Binomial distribution, Exponential distribution and Binomial distribution.\cr\cr

  The function chooses from plotting either PDF, CDF or both.\cr\cr

  The main purpose of the \code{plots} function is to visualize differences between some parameters given to the distribution functions.

}
\usage{
  plots(t, norm=TRUE, binom=FALSE, exp=FALSE, unif=FALSE, pdf=TRUE, cdf=FALSE)
}

\arguments{
  \item{t}{
      the interval which needs to be plotted on
  }
  \item{norm}{
      boolean - indicates that the wanted distribution is Normal
  }
  \item{binom}{
      boolean - indicates that the wanted distribution is Binomial
  }
  \item{exp}{
      boolean - indicates that the wanted distribution is Exponential
  }
  \item{unif}{
      boolean - indicates that the wanted distribution is Uniform
  }
  \item{pdf}{
      boolean - indicates that the wanted function is PDF (probability density function)
  }
  \item{cdf}{
      boolean - indicates that the wanted function is CDF (cumulative distribution function)
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
    The function doesn't return anything. It just plots the wanted distributions.
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
# the interval needs to be explicitly given as parameter
plots(seq(-2,5,0.001), exp=T, cdf=T)

# if no boolean parameter is set, then the function will not plot anything
plots(seq(-2,5,0.001))

# multiple boolean parameters can be set as TRUE
# the function will show multiple plots
plots(seq(-2,5,0.001), norm=T, exp=T, cdf=T, pdf=T)
}
