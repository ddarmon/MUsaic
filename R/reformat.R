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
