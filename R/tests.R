#' Welch's two-sample t-test with summary statistics
#'
#' Welch's two-sample t-test for testing claims about the different between two population means without assuming the population variances are equal.
#'
#' @param xbar the sample mean of the first sample.
#' @param ybar the sample mean of the second sample.
#' @param sx the sample standard deviation of the first sample.
#' @param sy the sample standard deviation of the second sample.
#' @param nx the sample size of the first sample.
#' @param ny the sample size of the second sample.
#' @param null.diff the assumed difference mu_X - mu_Y under the null hypothesis.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param conf.level confidence level for the interval estimator
#'
#' @export
two.sample.t.test = function(xbar, ybar, sx, sy, nx, ny, null.diff = 0,
                              alternative = c("two.sided", "less", "greater"),
                              var.equal = FALSE,
                              conf.level = 0.95){

  if (length(alternative) == 3){
    alternative = "two.sided"
  }

  alpha = 1 - conf.level

  if (var.equal){
    xbar.se = sx/sqrt(nx); ybar.se = sy/sqrt(ny)

    num = (xbar.se^2 + ybar.se^2)^2
    denom = (xbar.se^4)/(nx - 1) + (ybar.se^4)/(ny - 1)

    nu = num/denom

    se.diff = sqrt(sx^2/nx + sy^2/ny)
  }else{
    nu = nx + ny - 2

    sp = sqrt(((nx - 1)*sx^2 + (ny - 1)*sy^2)/(nx + ny - 2))

    se.diff = sp*sqrt(1/nx + 1/ny)
  }


  mean.diff = xbar - ybar

  num =  mean.diff - null.diff

  tobs = num/se.diff

  if (alternative == 'two.sided'){
    p.value = 2*pt(-abs(tobs), df = nu)

    conf.int = mean.diff + se.diff*qt(1-alpha/2, df = nu)*c(-1, 1)

    alt.text = sprintf("true difference in means is not equal to %g", null.diff)
  }else if (alternative == 'less'){
    p.value = pt(tobs, df = nu)

    conf.int = c(-Inf, mean.diff + se.diff*qt(conf.level, df = nu))

    alt.text = sprintf("true difference in means is less than %g", null.diff)
  }else if (alternative == 'greater'){
    p.value = 1 - pt(tobs, df = nu)

    conf.int = c(mean.diff - se.diff*qt(conf.level, df = nu), Inf)

    alt.text = sprintf("true difference in means is greater than %g", null.diff)
  }

  attr(conf.int, which = "conf.level") = conf.level

  names(tobs) <- "t"
  names(nu) <- "df"
  names(mean.diff) <- "mean(x) - mean(y)"
  wtt <- list(method = "Welch Two Sample t-test",
              data.name = sprintf('\nxbar = %g, sx = %g, nx = %g\nybar = %g, sy = %g, ny = %g\n', xbar, sx, nx, ybar, sy, ny),
              statistic = tobs,
              parameter = nu,
              p.value = p.value,
              conf.int = conf.int,
              estimate = mean.diff,
              alternative = alt.text)
  class(wtt) <- "htest"
  return(wtt)

}

#' Gosset's one-sample t-test with summary statistics
#'
#' Gosset's one-sample t-test for testing claims about a population mean.
#'
#' @param xbar the sample mean.
#' @param s the sample standard deviation.
#' @param n the sample size.
#' @param mu0 the assumed population mean under the null hypothesis.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param conf.level confidence level for the interval estimator
#'
#' @export
one.sample.t.test = function(xbar, s, n, mu0 = 0,
                        alternative = c("two.sided", "less", "greater"),
                        conf.level = 0.95){
  if (length(alternative) == 3){
    alternative = "two.sided"
  }

  alpha = 1 - conf.level

  nu = n - 1

  se = s/sqrt(n)

  tobs = (xbar - mu0)/se

  if (alternative == 'two.sided'){
    p.value = 2*pt(-abs(tobs), df = nu)

    conf.int = xbar + se*qt(1-alpha/2, df = nu)*c(-1, 1)

    alt.text = sprintf("true mean is not equal to %g", mu0)
  }else if (alternative == 'less'){
    p.value = pt(tobs, df = nu)

    conf.int = c(-Inf, xbar + se*qt(conf.level, df = nu))

    alt.text = sprintf("true mean is less than %g", mu0)
  }else if (alternative == 'greater'){
    p.value = 1 - pt(tobs, df = nu)

    conf.int = c(xbar - se*qt(conf.level, df = nu), Inf)

    alt.text = sprintf("true mean is greater than %g", mu0)
  }

  attr(conf.int, which = "conf.level") = conf.level

  names(tobs) <- "t"
  names(nu) <- "df"
  names(xbar) <- "mean of x"
  wtt <- list(method = "One Sample t-test",
              data.name = sprintf('\nxbar = %g, s = %g, n = %g\n', xbar, s, n),
              statistic = tobs,
              parameter = nu,
              p.value = p.value,
              conf.int = conf.int,
              estimate = xbar,
              alternative = alt.text)
  class(wtt) <- "htest"
  return(wtt)

}

#' @export
cor.test.exact = function(x, y, rho0 = 0,
                             alternative = c("two.sided", "less", "greater"),
                             conf.level = 0.95, exact = TRUE){

  if (exact == TRUE){
    pc <- pcorr
  }else{
    pc <- pcorr.fisher
  }

  if (length(alternative) == 3){
    alternative = "two.sided"
  }

  n <- length(x)

  stopifnot(length(x) == length(y))

  alpha = 1 - conf.level

  r <- cor(x, y)

  # Fudge factor here:
  # lower.eval <- -1+1e-3
  # upper.eval <- 1-1e-3

  approx.int <- cor.test(x, y, conf.level = conf.level, alternative = alternative)$conf.int

  if (alternative == 'two.sided'){
    p.value = 2*min(pc(r, rho0, n), pc(r, rho0, n, lower.tail = FALSE))

    # lb <- uniroot(function(rho) pc(r, rho, n, lower.tail = FALSE) - alpha/2, interval = c(lower.eval, upper.eval))$root
    # ub <- uniroot(function(rho) pc(r, rho, n) - alpha/2, interval = c(lower.eval, upper.eval))$root

    lb <- newtonRaphson(function(rho) pc(r, rho, n, lower.tail = FALSE) - alpha/2, x0 = approx.int[1])$root
    ub <- newtonRaphson(function(rho) pc(r, rho, n) - alpha/2, x0 = approx.int[2])$root

    conf.int = c(lb, ub)

    alt.text = sprintf("true correlation is not equal to %g", rho0)
  }else if (alternative == 'less'){
    p.value = pc(r, rho0, n)

    # ub <- uniroot(function(rho) pc(r, rho, n) - alpha, interval = c(lower.eval, upper.eval))$root
    ub <- newtonRaphson(function(rho) pc(r, rho, n) - alpha, x0 = approx.int[2])$root

    conf.int = c(-1,ub)

    alt.text = sprintf("true correlation is less than %g", rho0)
  }else if (alternative == 'greater'){
    p.value = pc(r, rho0, n, lower.tail = FALSE)

    # lb <- uniroot(function(rho) pc(r, rho, n, lower.tail = FALSE) - alpha, interval = c(lower.eval, upper.eval))$root
    lb <- newtonRaphson(function(rho) pc(r, rho, n, lower.tail = FALSE) - alpha, x0 = approx.int[1])$root

    conf.int = c(lb,1)

    alt.text = sprintf("true correlation is greater than %g", rho0)
  }

  attr(conf.int, which = "conf.level") = conf.level

  if (exact == TRUE){
    method.name <- "Exact bivariate Gaussian correlation test"
  }else{
    method.name <- "Fisher's z-transform bivariate Gaussian correlation test"
  }


  names(r) <- "r"
  wtt <- list(method = method.name,
              data.name = paste(deparse(substitute(x)), "and", deparse(substitute(y))),
              statistic = r,
              p.value = p.value,
              conf.int = conf.int,
              estimate = r,
              alternative = alt.text)
  class(wtt) <- "htest"
  return(wtt)
}
