dcorr <- function(x, rho, n){
  prefactor <- function(r, rho, n){
    (n - 2)*(1 - rho^2)^((n-1)/2)*(1 - r^2)^((n-4)/2)/pi
  }
  
  integrand <- function(w, r, rho, n){
    1/(cosh(w) - rho*r)^(n-1)
  }
  
  definite.integral <- function(r, rho, n){
    integrate(integrand, lower = 0, upper = Inf, r = r, rho = rho, n = n, rel.tol = 1e-7)$value
  }
  
  definite.integral <- Vectorize(definite.integral, vectorize.args = 'r')
  
  I <- definite.integral(x, rho, n)
  
  fn <- prefactor(x, rho, n)*I
  
  return(fn)
}

pcorr <- function(q, rho, n){
  integrate(dcorr, lower = -1, upper = q, rho = rho, n = n, rel.tol = 1e-7)$value
}

pcorr <- Vectorize(pcorr, vectorize.args = c('q', 'rho'))