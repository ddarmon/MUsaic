#' @export
columns_to_factors = function(data){
  col.names = colnames(data)

  response = c()
  label    = c()

  for (col.name in col.names){
    a = data[[col.name]]
    a = a[!is.na(a)]

    response = c(response, a)
    label = c(label, rep(col.name, length(a)))
  }

  data.new = data.frame(response = response, label = factor(label))

  return(data.new)
}

#' @export
frequency_table_from_gf_histogram = function(object, plot = TRUE){
  lc = ggplot_build(object)$data[[1]][, 4] # The left cutpoints
  rc = ggplot_build(object)$data[[1]][, 5] # The right cutpoints
  f  = ggplot_build(object)$data[[1]][, 2] # The frequencies

  bins = c()

  for (bin.ind in 1:length(lc)){
    if (bin.ind == 1){ # gf_histogram uses [a, b] for the first bin
      bin.text = sprintf('[%g, %g]', lc[bin.ind], rc[bin.ind])
    }else{ # gf_histogram uses (a, b] for all non-first bins
      bin.text = sprintf('(%g, %g]', lc[bin.ind], rc[bin.ind])
    }
    bins = c(bins, bin.text)
  }

  table = data.frame('left_boundary' = lc, 'right_boundary' = rc, 'bin' = bins, 'frequency' = f)

  if(plot){
    print(object)
  }

  return(table)
}
