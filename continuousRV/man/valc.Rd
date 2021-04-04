\name{valc}
\alias{valc}

\title{
    Creation of a continuous random variable
}

\description{
    The function creates a continuous random variable based on a \code{PDF} given as argument.
}

\usage{
valc(f)
}

\arguments{
  \item{f}{
  the probability density function of a continuous random variable to be created
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{

}

\references{
%% ~put references to the literature/web site here ~
}

\author{
Matei-Stefan Cibotaru
}

\note{
The generation time may be pretty long due to the usage of the \code{inverse} function from package GoFKernel.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
valc(dnorm)
}
