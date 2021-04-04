isPDF <- function(f, domain = NULL) {
  if (is.null(domain)) {
      message("Domain is required")
      return(NULL)
  }
  else {
      f <- Vectorize(f)
      ans <- TRUE

      # daca evaluarea functiei intr-un punct este infinita, inseamna ca a fost
      # realizata o impartire la 0, deci este nedefinita
      # daca functia este negativa intr-un punct al domeniului, atunci ea nu este pdf
      for (x in domain) {
        if (isTRUE(f(x) == Inf || f(x) == -Inf)) {
            ans <- NULL
            message("Function undefined on the given domain")
            break
        }
        if (isTRUE(f(x) < 0)) {
            ans <- FALSE
            break
        }
      }

      # se incearca calcularea integralei
      # ea trebuie sa fie egala cu 1 ca functia sa fie pdf
      # daca integrala este divergenta => eroare
      # daca integrala este != 1 => functia nu este pdf
      if (isTRUE(ans)) {
        INF <- Inf
        ans <- tryCatch(
          {
            I <- integrate(f, -INF, INF)
            TRUE
          },
          error = function(e) {
            message("Cannot check for pdf condition 2) - the integral is divergent")
            NULL
          }
        )

        if (!is.null(ans) && isTRUE(ans)) {
          if (isTRUE(I$value != 1.0)) {
                ans <- FALSE
          }
        }
      }

      return(ans)
  }

}
