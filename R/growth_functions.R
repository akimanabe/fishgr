vbgf <- function(age, p = list(Linf, K, t0)) {

  Linf <- as.numeric(p[1])
  K <- as.numeric(p[2])
  t0 <- as.numeric(p[3])

  Linf * (1 - exp(- K * (age - t0)))

}


