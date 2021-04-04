var_h <- function(h,pdf) {

  #dispersia este diferenta dintre media de x^2 unde am folosit functia mean_h si media de x la patrat

  a  <- mean_h(function(x){x^2}, pdf) - E(pdf)^2
  return(a)

}


