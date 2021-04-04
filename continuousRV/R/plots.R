plots <- function(t, norm=FALSE, binom=FALSE, exp=FALSE, unif=FALSE, pdf=FALSE, cdf=FALSE) {

  if(isTRUE(norm)) {
      if (isTRUE(pdf)) {
        plot(t, dnorm(t, tail(t,1)-1, 0.4), col="red", main="PDF", xlab="interval", ylab="dnorm")
        lines(t, dnorm(t, tail(t,1)-4, 0.55), col="blue", lwd=2)
        lines(t, dnorm(t, tail(t,1)/2, 0.7), col="green", lwd=2)
        lines(t, dnorm(t, tail(t,1)-1, 1), col="magenta", lwd=2)

        legend("topleft", inset=.05, title="Distributions", lwd=5, lty=1,
               col=c("red","blue","green","magenta"),
               legend=c(paste("dnorm(t, ",tail(t,1)-1,", 0.4)", sep=""),
                        paste("dnorm(t, ",tail(t,1)-4,", 0.55)", sep=""),
                        paste("dnorm(t, ",tail(t,1)/2,", 0.7)", sep=""),
                        paste("dnorm(t, ",tail(t,1)-1,", 1)", sep="")))
      }
      if (isTRUE(cdf)) {
        plot(t, pnorm(t, tail(t,1)-1, 0.4), col="red", main="CDF", xlab="interval", ylab="pnorm")
        lines(t, pnorm(t, tail(t,1)-4, 0.55), col="blue", lwd=2)
        lines(t, pnorm(t, tail(t,1)/2, 0.7), col="green", lwd=2)
        lines(t, pnorm(t, tail(t,1)-1, 1), col="magenta", lwd=2)

        legend("topleft", inset=.05, title="Distributions", lwd=5, lty=1,
               col=c("red","blue","green","magenta"),
               legend=c(paste("pnorm(t, ",tail(t,1)-1,", 0.4)", sep=""),
                        paste("pnorm(t, ",tail(t,1)-4,", 0.55)", sep=""),
                        paste("pnorm(t, ",tail(t,1)/2,", 0.7)", sep=""),
                        paste("pnorm(t, ",tail(t,1)-1,", 1)", sep="")))
      }
  }
  if(isTRUE(binom)) {
      if (isTRUE(pdf)) {
        plot(head(t,1) : tail(t,1), dbinom(head(t,1) : tail(t,1), 5, 0.4), col="red", main="PDF", xlab="interval", ylab="dbinom")
        lines(head(t,1) : tail(t,1), dbinom(head(t,1) : tail(t,1), 5, 0.7), col="magenta", lwd=2)
        lines(head(t,1) : tail(t,1), dbinom(head(t,1) : tail(t,1), 3, 0.4), col="green", lwd=2)
        lines(head(t,1) : tail(t,1), dbinom(head(t,1) : tail(t,1), 9, 1), col="blue", lwd=2)

        legend("topleft", inset=.05, title="Distributions", lwd=5, lty=1,
               col=c("red","magenta","green","blue"),
               legend=c(paste("dbinom(",head(t,1),":",tail(t,1),", 5, 0.4)", sep=""),
                        paste("dbinom(",head(t,1),":",tail(t,1),", 5, 0.7)", sep=""),
                        paste("dbinom(",head(t,1),":",tail(t,1),", 3, 0.4)", sep=""),
                        paste("dbinom(",head(t,1),":",tail(t,1),", 9, 1)", sep="")))
      }
      if (isTRUE(cdf)) {
        plot(head(t,1) :tail(t,1), pbinom(head(t,1) : tail(t,1), 5, 0.4), col="red", main="CDF", xlab="interval", ylab="pbinom")
        lines(head(t,1) : tail(t,1), pbinom(head(t,1) : tail(t,1), 5, 0.7), col="red", lwd=2)
        lines(head(t,1) : tail(t,1), pbinom(head(t,1) : tail(t,1), 3, 0.4), col="black", lwd=2)
        lines(head(t,1) : tail(t,1), pbinom(head(t,1) : tail(t,1), 9, 1), col="blue", lwd=2)

        legend("topleft", inset=.05, title="Distributions", lwd=5, lty=1,
               col=c("red","magenta","green","blue"),
               legend=c(paste("pbinom(",head(t,1),":",tail(t,1),", 5, 0.4)", sep=""),
                        paste("pbinom(",head(t,1),":",tail(t,1),", 5, 0.7)", sep=""),
                        paste("pbinom(",head(t,1),":",tail(t,1),", 3, 0.4)", sep=""),
                        paste("pbinom(",head(t,1),":",tail(t,1),", 9, 1)", sep="")))
      }
  }
  if(isTRUE(exp)) {
      if (isTRUE(pdf)) {
        plot(t, dexp(t, 1), col="gold", main="PDF", xlab="interval", ylab="dexp")
        lines(t, dexp(t, 1/2), col="red", lwd=2)
        lines(t, dexp(t,5), col="blue", lwd=2)
        lines(t, dexp(t,0.25), col="green", lwd=2)

        legend("topright", inset=.05, title="Distributions", lwd=5, lty=1,
               col=c("gold","red","blue","green"),
               legend=c(paste("dexp(t, 1)", sep=""),
                        paste("dexp(t, 0.5)", sep=""),
                        paste("dexp(t, 5)", sep=""),
                        paste("dexp(t, 0.25)", sep="")))
      }
      if (isTRUE(cdf)) {
        plot(t, pexp(t, 1), col="gold", main="CDF", xlab="interval", ylab="pexp")
        lines(t, pexp(t, 1/2), col="red", lwd=2)
        lines(t, pexp(t,5), col="blue", lwd=2)
        lines(t, pexp(t,0.25), col="green", lwd=2)

        legend("bottomright", inset=.05, title="Distributions", lwd=5, lty=1,
               col=c("gold","red","blue","green"),
               legend=c(paste("pexp(t, 1)", sep=""),
                        paste("pexp(t, 0.5)", sep=""),
                        paste("pexp(t, 5)", sep=""),
                        paste("pexp(t, 0.25)", sep="")))
      }
  }
  if(isTRUE(unif)) {
      if (isTRUE(pdf)) {
        plot(t,  dunif(t, min=-head(t,1), max=tail(t,1)-1), col="red", main="PDF", xlab="interval", ylab="dunif")
        lines(t, dunif(t, 1/2, 5), col="pink", lwd=3)
        lines(t, dunif(t, head(t,1)/2, tail(t,1)/2), col="purple", lwd=2)
        lines(t, dunif(t, head(t,1)/5, tail(t,1)/5), col="blue", lwd=2)

        legend("right", inset=.05, title="Distributions", lwd=5, lty=1,
               col=c("red","pink","purple","blue"),
               legend=c(paste("dunif(t, ",-head(t,1),", ",tail(t,1)-1,")", sep=""),
                        paste("dunif(t, 0.5, 5)", sep=""),
                        paste("dunif(t, ",head(t,1)/2,", ",tail(t,1)/2,")", sep=""),
                        paste("dunif(t, ",head(t,1)/5,", ",tail(t,1)/5,")", sep="")))
      }
      if (isTRUE(cdf)) {
        plot(t,  punif(t, min=-head(t,1), max=tail(t,1)-1), col="red", main="CDF", xlab="interval", ylab="punif")
        lines(t, punif(t, 1/2, 5), col="pink", lwd=3)
        lines(t, punif(t, head(t,1)/2, tail(t,1)/2), col="purple", lwd=2)
        lines(t, punif(t, head(t,1)/5, tail(t,1)/5), col="blue", lwd=2)

        legend("bottomright", inset=.05, title="Distributions", lwd=5, lty=1,
               col=c("red","pink","purple","blue"),
               legend=c(paste("punif(t, ",-head(t,1),", ",tail(t,1)-1,")", sep=""),
                        paste("punif(t, 0.5, 5)", sep=""),
                        paste("punif(t, ",head(t,1)/2,", ",tail(t,1)/2,")", sep=""),
                        paste("punif(t, ",head(t,1)/5,", ",tail(t,1)/5,")", sep="")))
      }
  }
}
