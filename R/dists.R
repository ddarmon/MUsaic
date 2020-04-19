library(cubature)

dcorr <- function(x, rho, n){
  prefactor <- function(r, rho, n){
    (n - 2)*(1 - rho^2)^((n-1)/2)*(1 - r^2)^((n-4)/2)/pi
  }
  
  integrand <- function(w, r, rho, n){
    1/(cosh(w) - rho*r)^(n-1)
  }
  
  definite.integral <- function(r, rho, n){
    cubintegrate(integrand, lower = 0, upper = Inf, nVec = 1, relTol = 1e-30, method = "pcubature", r = r, rho = rho, n = n)$integral
    # integrate(integrand, lower = 0, upper = Inf, r = r, rho = rho, n = n, rel.tol = 1e-20)$value
  }
  definite.integral <- Vectorize(definite.integral, vectorize.args = 'r')
  
  I <- definite.integral(x, rho, n)
  
  fn <- prefactor(x, rho, n)*I
  
  return(fn)
}

pcorr <- function(q, rho, n, lower.tail = TRUE){
  if (lower.tail){
    cubintegrate(dcorr, lower = -1, upper = q, method = "pcubature", nVec = 1, relTol = 1e-30, rho = rho, n = n)$integral
  }else{
    cubintegrate(dcorr, lower = q, upper = 1, method = "pcubature", nVec = 1, relTol = 1e-30, rho = rho, n = n)$integral
  }
  # integrate(dcorr, lower = -1, upper = q, rho = rho, n = n, rel.tol = 1e-7)$value
}

pcorr <- Vectorize(pcorr, vectorize.args = c('q', 'rho'))