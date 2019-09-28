#' @export
two.sample.t.test = function(xbar, ybar, sx, sy, nx, ny, null.diff = 0,
                              alternative = c("two.sided", "less", "greater"),
                              conf.level = 0.95){

  alpha = 1 - conf.level

  xbar.se = sx/sqrt(nx); ybar.se = sy/sqrt(ny)

  num = (xbar.se^2 + ybar.se^2)^2
  denom = (xbar.se^4)/(nx - 1) + (ybar.se^4)/(ny - 1)

  nu = num/denom

  mean.diff = xbar - ybar

  num =  mean.diff - null.diff
  se.diff = sqrt(sx^2/nx + sy^2/ny)

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

#' @export
one.sample.t.test = function(xbar, s, n, mu0 = 0,
                        alternative = c("two.sided", "less", "greater"),
                        conf.level = 0.95){

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
