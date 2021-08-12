library(arm)

a = 2
b  = 0 
Ybar  = 0.5

calc_p <- function(alpha = a, beta = b, mu = Ybar) {
  (1 - mu) * invlogit(alpha) +
    mu * invlogit(alpha + beta)
}

calc_rho <- function(alpha = a, beta = b, mu = Ybar) {
  p  = calc_p(alpha, beta)
  
  constant = invlogit(alpha + beta)  - invlogit(alpha)
  
  constant * sqrt(mu * (1 - mu)) / sqrt(p * (1 - p))
}

calc_p(alpha = 1,     beta = 0, mu = 0.5)
calc_p(alpha = 0.56, beta = 1, mu = 0.5)

calc_p(alpha = -3, beta = 0, mu = 0.5)
calc_p(alpha = -3.8, beta = 1, mu = 0.5)
