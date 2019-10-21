plot_points_with_smoothing_spline = function(x, y, xname = 'x', yname = 'Residuals', yline){
  if (length(unique(x)) >= 4){
    ss.out = smooth.spline(x, y, cv = TRUE)
    
    xrange = range(ss.out$x)
    
    xpred = seq(xrange[1], xrange[2], length.out = 200)
    sspred = predict(ss.out, x = xpred)
    
    return(gf_point(y ~ x) %>% gf_line(sspred$y ~ sspred$x, col = 'red', lwd = 2) %>% gf_hline(yintercept = ~ yline, lty = 2) %>% gf_labs(x = xname, y = yname)) 
  }else{
    return(gf_point(y ~ x) %>% gf_hline(yintercept = ~ yline, lty = 2) %>% gf_labs(x = xname, y = yname))
  }
}

gf_residuals_versus_predictors = function(object, squared = FALSE){
  predictor.names = names(object$coefficients)
  
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
  
  print(plot_points_with_smoothing_spline(object$fitted.values, rs, xname = 'Fitted Values', yname = rs.text, yline = yline), newpage = TRUE)
  
  for (predictor.name in predictor.names){
    if (predictor.name != "(Intercept)"){
      print(plot_points_with_smoothing_spline(object$model[[predictor.name]], rs, xname = predictor.name, yname = rs.text, yline = yline), newpage = TRUE)
    }
  }
}