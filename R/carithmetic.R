#' Raise an integer to an integer power
#'
#' Raise a fixed-point integer to a fixed-point integer power,
#' while maintaining the numeric type of the integer, returning
#'
#' base^exponent
#'
#' This is needed since in R, a^b, with a and b both integer,
#' returns a float rather than an integer.
#'
#' @param base the integer base of the exponential
#' @param exponent the integer exponent of the exponential
integer.to.integer.power <- function(base, exponent){
  pow <- 1L

  for (k in 1:exponent){
    pow <- pow*base
  }

  return(pow)
}

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
#' This function returns the
#'
#' **NOTE:** Both d and e should be **integer type**, i.e.
#' suffixed with an `L`.
#'
#' If not, they will be coerced to integer type if possible.
#'
#' @param d the integer part of the decimal fraction.
#' @param e the exponent for 10^(-e)
#' @param ndigits the maximum number of digits to search for the repeating pattern
#'
#' @examples
#' # Binary expansion of 1/10 = 1/10^1
#'
#' convert.decimal.fraction.to.binary(1, 1)
#'
#' # Binary expansion of 1/2 = 5/10^1
#'
#' convert.decimal.fraction.to.binary(5, 1)
#'
#' # Binary expansion of 1/100 = 1/10^2
#'
#' convert.decimal.fraction.to.binary(1, 2)
#' @export
convert.decimal.fraction.to.binary <- function(d, e, ndigits = 54){
  # Only works for decimal fractions.
  stopifnot(e >= 1)

  # Make sure d < 10^e

  stopifnot(d < 10^e)

  # Check that d and e are integer type.
  # Otherwise convert to integer type.

  if (!is.integer(d)){
    stopifnot(all.equal(d - floor(d), 0L))

    d <- as.integer(d)
  }

  if (!is.integer(e)){
    stopifnot(all.equal(e - floor(e), 0L))

    e <- as.integer(e)
  }

  # Collector for the remainders from
  #
  #     2*d mod 10^e
  #
  # for finding when the remainder repeats (if it does).

  as <- c()

  a <- d
  as <- c(as, a)

  # Collector for the final string-version of the binary
  # expansion.

  string <- ''

  expansion <- rep(0, ndigits)
  for (i in 1:ndigits) {
    b <- 2L * a
    if (log10(b) >= e) {
      lead <- 1L
      a <- b - integer.to.integer.power(10L, e)
    }
    else {
      lead <- 0L
      a <- b
    }
    expansion[i] <- lead

    # Find where the remainder has been seen, if it all.

    where.seen <- which(as == a)

    if (length(where.seen) > 0){ # We are repeating, so can truncate and give the repeating part

      expansion <- expansion[1:i]

      if (where.seen == 1){ # The repeating part starts immediately after the decimal
        string <- paste0("0.(",
                         paste0(expansion[where.seen:i], collapse = ''),
                         ")", collapse = '')

        initial.part <- c()
        repeating.part <- expansion[where.seen:i]
      }else{ # There are non-repeating parts before the decimal
        string <- paste0("0.",
                         paste0(expansion[1:(where.seen-1)], collapse = ''),
                         "(",
                         paste0(expansion[where.seen:i], collapse = ''),
                         ")", collapse = '')

        initial.part <- expansion[1:(where.seen-1)]
        repeating.part <- expansion[where.seen:i]
      }

      break
    }

    as <- c(as, a)

    if (identical(a, 0L)) { # Check if the binary expansion has terminated.
      expansion <- expansion[1:i]
      string <- paste0("0.", expansion, collapse = '')

      initial.part <- expansion[1:i]
      repeating.part <- c()

      break
    }
  }

  if (string == ''){ # If the binary expansion hasn't repeated / terminated, indicate that
                     # the binary expansion hasn't been found by terminal '(...)'
    string <- paste0("0.",
                     paste0(expansion, collapse = ''),
                     '(...)',
                     collapse = '')

    initial.part <- expansion
    repeating.part <- c()
  }

  return(list(expansion = expansion, string = string, initial.part = initial.part, repeating.part = repeating.part, terms = as))
}
