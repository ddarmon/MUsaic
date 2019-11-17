gf_pop_props = function(x, conf.level = 0.95, p = NULL){
  alpha = 1 - conf.level
  ci.out = multinomialCI(x, alpha = alpha)
  
  if (!is.null(p)){
    est = data.frame(phat = x / sum(x), ci.l = ci.out[, 1], ci.u = ci.out[, 2], p = p)
  }else{
    est = data.frame(phat = x / sum(x), ci.l = ci.out[, 1], ci.u = ci.out[, 2])
  }
  
  if (!is.null(p)){
    g = gf_point(phat ~ 1:nrow(est), data = est, cex = 3) %>%
      gf_errorbar(ci.l + ci.u ~ 1:nrow(est), data = est) %>%
      gf_labs(x = 'Category Number', y = 'Population Proportion') %>%
      gf_lims(y = c(0, 1)) %>%
      gf_point(p ~ (1:nrow(est) + 0.1), col = 'red', pch = 2, cex = 2)
  }else{
    g = gf_point(phat ~ 1:nrow(est), data = est, cex = 3) %>%
      gf_errorbar(ci.l + ci.u ~ 1:nrow(est), data = est) %>%
      gf_labs(x = 'Category Number', y = 'Population Proportion') %>%
      gf_lims(y = c(0, 1))
  }
  print(g)
  
  return(est)
}