plot_points_with_smoothing_spline = function(x, y, xname = 'x', yname = 'Residuals', yline){
  if (length(unique(x)) >= 4){
    ss.out = smooth.spline(x, y, cv = TRUE)

    xrange = range(ss.out$x)

    xpred = seq(xrange[1], xrange[2], length.out = 200)
    sspred = predict(ss.out, x = xpred)

    return(gf_point(y ~ x) %>% gf_line(sspred$y ~ sspred$x, col = 'red', lwd = 1) %>% gf_hline(yintercept = ~ yline, lty = 2) %>% gf_labs(x = xname, y = yname))
  }else{
    return(gf_point(y ~ x) %>% gf_hline(yintercept = ~ yline, lty = 2) %>% gf_labs(x = xname, y = yname))
  }
}

plot_points_with_gam = function(x, y, xname = 'x', yname = 'Residuals', yline){
    tryCatch(
      expr = {
        gam.out = mgcv::gam(y ~ s(x))

        xrange = range(x)

        xpred = seq(xrange[1], xrange[2], length.out = 200)

        gampred = predict(gam.out, newdata = data.frame(x = xpred))

        return(gf_point(y ~ x) %>% gf_line(gampred ~ xpred, col = 'red', lwd = 1) %>% gf_hline(yintercept = ~ yline, lty = 2) %>% gf_labs(x = xname, y = yname))
      },
      error = function(e){
        return(gf_point(y ~ x) %>% gf_hline(yintercept = ~ yline, lty = 2) %>% gf_labs(x = xname, y = yname))
      })
}

gf_residuals_versus_predictors = function(object, squared = FALSE){
  X = model.matrix(object)
  predictor.names = colnames(X)

  if (predictor.names[1] == '(Intercept)'){
    p = ncol(X) - 1
    predictor.names = predictor.names[2:(p+1)]
    intercept.offset = 1
  }else{
    p = ncol(X)

    intercept.offset = 0
  }

  rs = object$residuals

  if (squared){
    rs.text = expression('Residuals'^2)
    yline = summary(object)$sigma^2
  }else{
    rs.text = 'Residuals'
    yline = 0
  }

  if (squared){
    rs = rs^2
  }

  print(plot_points_with_gam(object$fitted.values, rs, xname = 'Fitted Values', yname = rs.text, yline = yline), newpage = TRUE)

  if (p > 0){
    for (j in 1:p){
      predictor.name = predictor.names[j]
        print(plot_points_with_gam(X[, intercept.offset+j], rs, xname = predictor.name, yname = rs.text, yline = yline), newpage = TRUE)
    }
  }
}
