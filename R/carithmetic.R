#' Convert a decimal fraction into a binary fraction using
#' integer arithmetic with Horner's method.
#'
#' A function to convert a decimal fraction into a binary
#' fraction, for demonstration of rounding necessary to
#' represent most fractional numbers on a computer.
#'
#' See
#'
#' http://cs.furman.edu/digitaldomain/more/ch6/dec_frac_to_bin.htm
#'
#' for more details.
#'
#' @param d the integer part of the decimal fraction.
#' @param e the exponent for 10^(-e)
#' @param ndigits the number of digits to return
convert.decimal.fraction.to.binary <- function(d, e, ndigits = 54){
  a <- d

  expansion <- rep(0, ndigits)

  for (i in 1:ndigits){
    b <- 2L*a

    if (log10(b) >= e){
      lead <- 1L

      a <- b - 10L^e
    }else{
      lead <- 0L

      a <- b
    }

    expansion[i] <- lead

    if (identical(a, 0L)){
      break
    }
  }

  return(expansion)
}
