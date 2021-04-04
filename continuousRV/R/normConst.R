normConst <- function(f, domain = NULL) {

  if (is.null(domain)) {
    message("Domain is required")
    return(NULL)
  }
  else {
    ok <- TRUE

    #verificarea ca functia sa fie pozitiva pe tot domeniul
    for (x in domain) {
      if (f(x) < 0) {
        message("Normalization constant cannot be determined")
        ok <- FALSE
        return(NULL)
      }
    }

    if (isTRUE(ok)) {
        I <- integrate(f, head(domain, 1), tail(domain, 1))
        I <- I$value

        # daca integrala este nula, nu poate fi determinata constanta
        # altfel, constanta este calculata matematic
        if (isTRUE(I == 0)) {
          message("Normalization constant cannot be determined")
          return(NULL)
        }
        else {
          k <- 1 / I
          return(k)
        }
    }
  }
}
