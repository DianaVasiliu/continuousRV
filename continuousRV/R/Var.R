Var <- function(pdf, ...) {
  # varianta este echivalenta cu momentul centrat de ordin 2
  mu <- E(pdf, ...)
  cmoment(pdf, 2, mu)
}
