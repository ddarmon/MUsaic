#' Convert columns to factors.
#' 
#' Function to convert a data frame with two or more columns of numerical values, where each column corresponds to a specific factor level, into two columns, where the first column contains all of the numerical values and the second column contains their corresponding factor level.
#'
#' @param data the data frame containing the (two or more) numerical columns.
#' @param col.names.use a vector containing the desired column names as strings.
columns_to_factors = function(data, col.names.use = NULL){
  col.names = colnames(data)
  
  if (is.null(col.names.use)){
    col.names.use = col.names
  }

  response = c()
  label    = c()

  for (col.ind in 1:ncol(data)){
    col.name = col.names[col.ind]
    
    a = data[[col.name]]
    a = a[!is.na(a)]

    response = c(response, a)
    label = c(label, rep(col.names.use[col.ind], length(a)))
  }

  data.new = data.frame(response = response, label = factor(label))

  return(data.new)
}

#' Generate a frequency table from a ggformula histogram object.
#' 
#' Function to create a frequency table from a ggformula histogram object. Returns the cut points, bins, and counts within each bin.
#' 
#' @param object An output from gf_histogram.
#'
#' @param plot Logical indicating whether (TRUE) or not (FALSE) to plot the histogram passed to frequency_table_from_gf_histogram.
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

#' @export
string2matrix = function(s){
  rows = strsplit(s, ';')[[1]]
  
  A = c()
  
  for (row in rows){
    entries = strsplit(trimws(row), ' ')[[1]]
    
    A = c(A, entries)
  }
  
  An = as.numeric(A)
  
  mat = matrix(An, nrow = length(rows), ncol = length(entries), byrow = TRUE)
  
  return(mat)
}