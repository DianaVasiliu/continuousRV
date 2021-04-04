mean_h <- function(h, pdf) {

  #media functiei de X, Y=h(x) este o variabila aleatoare si E(Y)=(integrala -inf,inf h(x)*f(x))

  v <- integrate(Vectorize(function(x){
      h(x) * pdf(x)
  }), -Inf, Inf)

  return(v$value)
}


