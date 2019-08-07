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
  table = ggplot_build(object)$data[[1]][, c(4:5, 2)]

  colnames(table) = c('left_cut', 'right_cut', 'frequency')

  if(plot){
    print(object)
  }

  return(table)
}
