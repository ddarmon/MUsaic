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

#' Get out the leading (left-most) digit in the base-10
#' representation of an integer / float.
#'
#' @param number an integer / float
first.decimal.digit <- function(number){
  floor(number / 10^floor(log10(number)))
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

  binary.representation <- ''

  expansion <- rep(0, ndigits)
  for (i in 1:ndigits) {
    b <- 2L * a
    if (log10(b) >= e) {
      lead <- 1L
      a <- b - integer.to.integer.power(10L, e)
    } else {
      lead <- 0L
      a <- b
    }
    expansion[i] <- lead

    # Find where the remainder has been seen, if at all.

    where.seen <- which(as == a)

    if (length(where.seen) > 0){ # We are repeating, so can truncate and give the repeating part

      expansion <- expansion[1:i]

      if (where.seen == 1){ # The repeating part starts immediately after the decimal
        binary.representation <- paste0("0.(",
                         paste0(expansion[where.seen:i], collapse = ''),
                         ")", collapse = '')

        initial.part <- c()
        repeating.part <- expansion[where.seen:i]
      }else{ # There are non-repeating parts before the decimal
        binary.representation <- paste0("0.",
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
      binary.representation <- paste0("0.", expansion, collapse = '')

      initial.part <- expansion[1:i]
      repeating.part <- c()

      break
    }
  }

  if (binary.representation == ''){ # If the binary expansion hasn't repeated / terminated, indicate that
                     # the binary expansion hasn't been found by terminal '(...)'
    binary.representation <- paste0("0.",
                     paste0(expansion, collapse = ''),
                     '(...)',
                     collapse = '')

    initial.part <- expansion
    repeating.part <- c()
  }

  # Get out decimal representation
  #
  # NOTE:
  #
  #   d x 10^(-e) where d in 0, 1, 2, ..., 10^{e} - 1
  #
  # log10(d) - e

  num.leading.zeros <- ceiling(e - log10(d)) - 1

  decimal.representation <- paste0('0.',
         paste0(rep(0, num.leading.zeros), collapse = ''),
         d)

  return(list(decimal.representation = decimal.representation, binary.representation = binary.representation, expansion = expansion, initial.part = initial.part, repeating.part = repeating.part, terms = as))
}

#' Convert a decimal fraction into its base-n (n-nary) representation using
#' integer arithmetic with Horner's method.
#'
#' A function to convert a decimal fraction into its base-n
#' (n-ary) representation.
#'
#' See
#'
#' http://cs.furman.edu/digitaldomain/more/ch6/dec_frac_to_bin.htm
#'
#' for more details.
#'
#' **NOTE:** d, e, and base should be **integer type**, i.e.
#' suffixed with an `L`.
#'
#' If not, they will be coerced to integer type if possible.
#'
#' @param d the integer part of the decimal fraction.
#' @param e the exponent for 10^(-e)
#' @param base the base for the n-ary representation of the decimal fraction
#' @param ndigits the maximum number of digits to search for the repeating pattern
#'
#' @examples
#' # Binary expansion of 1/10 = 1/10^1
#'
#' convert.decimal.fraction.to.nary(1, 1, base = 2)
#'
#' # Trinary expansion of 1/10 = 1/10^1
#'
#' convert.decimal.fraction.to.nary(5, 1, base = 3)
#'
#' # Base-5 expansion of 1/5 = 2/10^1
#'
#' convert.decimal.fraction.to.nary(2, 1, base = 5)
#' @export
convert.decimal.fraction.to.nary <- function(d, e, base, ndigits = 54){
  # Only works for decimal fractions.
  stopifnot(e >= 1)

  # Make sure d < 10^e

  stopifnot(d < 10^e)

  # Check that d, e, and base are integer type.
  # Otherwise convert to integer type.

  if (!is.integer(d)){
    stopifnot(all.equal(d - floor(d), 0L))

    d <- as.integer(d)
  }

  if (!is.integer(e)){
    stopifnot(all.equal(e - floor(e), 0L))

    e <- as.integer(e)
  }

  if (!is.integer(base)){
    stopifnot(all.equal(base - floor(base), 0L))

    base <- as.integer(base)
  }

  # Collector for the remainders from
  #
  #     base*d mod 10^e
  #
  # for finding when the remainder repeats (if it does).

  as <- c()

  a <- d
  as <- c(as, a)

  # Collector for the final string-version of the n-ary
  # expansion.

  nary.representation <- ''

  expansion <- rep(0, ndigits)
  for (i in 1:ndigits) {
    b <- base * a

    if (log10(b) >= e) {
      lead <- first.decimal.digit(b)
      a <- b %% integer.to.integer.power(10L, e)
    } else {
      lead <- 0L
      a <- b
    }
    expansion[i] <- lead

    # Find where the remainder has been seen, if at all.

    where.seen <- which(as == a)

    if (length(where.seen) > 0){ # We are repeating, so can truncate and give the repeating part

      expansion <- expansion[1:i]

      if (where.seen == 1){ # The repeating part starts immediately after the decimal
        nary.representation <- paste0("0.(",
                                      paste0(expansion[where.seen:i], collapse = ''),
                                      ")", collapse = '')

        initial.part <- c()
        repeating.part <- expansion[where.seen:i]
      }else{ # There are non-repeating parts before the decimal
        nary.representation <- paste0("0.",
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

    if (identical(a, 0L)) { # Check if the n-ary expansion has terminated.
      expansion <- expansion[1:i]
      nary.representation <- paste0("0.",
                                    paste0(expansion, collapse = ''),
                                    collapse = '')

      initial.part <- expansion[1:i]
      repeating.part <- c()

      break
    }
  }

  if (nary.representation == ''){ # If the n-ary expansion hasn't repeated / terminated, indicate that
                                  # the n-ary expansion hasn't been found by terminal '(...)'
    nary.representation <- paste0("0.",
                                  paste0(expansion, collapse = ''),
                                  '(...)',
                                  collapse = '')

    initial.part <- expansion
    repeating.part <- c()
  }

  # Get out decimal representation
  #
  # NOTE:
  #
  #   d x 10^(-e) where d in 0, 1, 2, ..., 10^{e} - 1
  #
  # log10(d) - e

  num.leading.zeros <- ceiling(e - log10(d)) - 1

  decimal.representation <- paste0('0.',
                                   paste0(rep(0, num.leading.zeros), collapse = ''),
                                   d)

  return(list(base = base, decimal.representation = decimal.representation, nary.representation = nary.representation, expansion = expansion, initial.part = initial.part, repeating.part = repeating.part, terms = as))
}

#' Convert a rational fraction into its base-n (n-nary) representation using
#' integer arithmetic with Horner's method.
#'
#' A function to convert a rational fraction into its base-n
#' (n-ary) representation.
#'
#' See
#'
#' http://cs.furman.edu/digitaldomain/more/ch6/dec_frac_to_bin.htm
#'
#' for more details.
#'
#' **NOTE:** p, q, and base should be **integer type**, i.e.
#' suffixed with an `L`.
#'
#' If not, they will be coerced to integer type if possible.
#'
#' @param p the numerator of the rational fraction
#' @param q the denominator of the rational fraction
#' @param base the base for the n-ary representation of the decimal fraction
#' @param ndigits the maximum number of digits to search for the repeating pattern
#'
#' @examples
#' # Binary expansion of 1/10
#'
#' convert.rational.fraction.to.nary(1, 10, base = 2)
#'
#' # Base-5 expansion of 1/3
#'
#' convert.decimal.fraction.to.nary(1, 3, base = 5)
#' @export
convert.rational.fraction.to.nary <- function(p, q, base, ndigits = 1000){
  # Only works for rational fractions.

  stopifnot(p/q < 1)

  # Only works for up to decimal representation.

  stopifnot(base > 0 & base <= 10)

  # Check that p, q, and base are integer type.
  # Otherwise convert to integer type.

  if (!is.integer(p)){
    stopifnot(all.equal(p - floor(p), 0L))

    p <- as.integer(p)
  }

  if (!is.integer(q)){
    stopifnot(all.equal(q - floor(q), 0L))

    q <- as.integer(q)
  }

  if (!is.integer(base)){
    stopifnot(all.equal(base - floor(base), 0L))

    base <- as.integer(base)
  }

  expansion <- rep(NA, ndigits)

  nary.representation <- ""

  b <- p

  bs.seen <- c()

  for (i in 1:ndigits){
    b <- base*b

    where.b <- which(bs.seen == b)

    if (length(where.b) > 0){
      expansion <- expansion[1:(i-1)]

      if (where.b == 1) { # The repeating part starts immediately after the decimal
        nary.representation <- paste0("0.(", paste0(expansion[where.b:(i-1)],
                                                    collapse = ""), ")", collapse = "")
        initial.part <- c()
        repeating.part <- expansion[where.b:(i-1)]
      } else { # There are non-repeating parts before the decimal
        nary.representation <- paste0("0.",
                                      paste0(expansion[1:(where.b -
                                                            1)], collapse = ""),
                                      "(", paste0(expansion[where.b:(i-1)],
                                                  collapse = ""),
                                      ")", collapse = "")
        initial.part <- expansion[1:(where.b - 1)]
        repeating.part <- expansion[where.b:(i-1)]
      }

      break
    }else{
      bs.seen <- c(bs.seen, b)
    }

    expansion[i] <- as.integer(floor(b/q))

    b <- b - expansion[i]*q
  }

  if (nary.representation == "") { # If the n-ary expansion hasn't repeated / terminated, indicate that
    # the n-ary expansion hasn't been found by terminal '(...)'
    nary.representation <- paste0("0.", paste0(expansion,
                                               collapse = ""), "(...)", collapse = "")
    initial.part <- expansion
    repeating.part <- c()
  }

  return(list(p = p, q = q,
              nary.representation = nary.representation, expansion = expansion,
              initial.part = initial.part, repeating.part = repeating.part,
              terms = bs.seen))
}

#' Convert a rational fraction into its base-n representation using Horner's method and GMP library.
#'
#' This function computes the representation of a rational fraction in a specified base (n-ary form),
#' using the GNU Multiple Precision (GMP) library for arbitrary precision arithmetic. The process involves
#' repeated division by the base, using Horner's method for efficient computation. The function detects
#' and marks repeating sequences in the n-ary expansion.
#' 
#' `gmp::as.bigz()` is used to allow for arbitrarily large integers for the
#' numerator and denominator of the rational fraction.
#'
#' For more theoretical background, see:
#' http://cs.furman.edu/digitaldomain/more/ch6/dec_frac_to_bin.htm
#'
#' @param p A `bigz` object representing the numerator of the fraction.
#' @param q A `bigz` object representing the denominator of the fraction.
#' @param base The base in which to express the fraction.
#' @param ndigits The maximum number of digits to compute before stopping or detecting a repeat.
#'
#' @examples
#' library(gmp)
#' # Convert the fraction 1/9007199254740992 to binary
#' convert_rational_fraction_to_nary_gmp(as.bigz("1"), as.bigz("9007199254740992"), base = 2)
#'
#' @return A list containing:
#' - `p`: The numerator.
#' - `q`: The denominator.
#' - `nary_representation`: The n-ary number as a string.
#' - `expansion`: Numeric vector of computed digits.
#' - `initial_part`: Non-repeating part of the expansion.
#' - `repeating_part`: Repeating sequence of digits in the expansion.
#' - `terms`: List of intermediary terms used in computation.
#' @export
convert_rational_fraction_to_nary <- function(p, q, base, ndigits = 1000) {
  require(gmp)  # Load GMP library for big integer arithmetic
  
  # Ensure all inputs are of type bigz
  p <- as.bigz(p)
  q <- as.bigz(q)
  base <- as.bigz(base)
  
  # Initialize variables for the computation
  expansion <- numeric(ndigits)
  bs_seen <- list()
  nary_representation <- ""
  b <- p
  
  # Compute the n-ary expansion
  for (i in 1:ndigits) {
    b <- base * b
    found_index <- -1
    
    # Check if this term has been seen before (detecting cycles)
    for (j in seq_along(bs_seen)) {
      if (isTRUE(all.equal(bs_seen[[j]], b))) {
        found_index <- j
        break
      }
    }
    
    if (found_index > 0) {  # A repeating sequence is found
      expansion <- expansion[1:(i - 1)]
      if (found_index == 1) {
        nary_representation <- sprintf("0.(%s)", paste(expansion, collapse = ""))
      } else {
        nary_representation <- sprintf("0.%s(%s)", 
                                       paste(expansion[1:(found_index - 1)], collapse = ""),
                                       paste(expansion[found_index:(i - 1)], collapse = ""))
      }
      break
    } else {
      bs_seen[[length(bs_seen) + 1]] <- b
    }
    
    # Compute and record the digit
    expansion[i] <- as.integer(as.bigz(floor(b / q)))
    b <- b - as.bigz(expansion[i]) * q
  }
  
  # Construct output for non-repeating case
  if (nary_representation == "") {
    nary_representation <- sprintf("0.%s(...)", paste(expansion, collapse = ""))
  }
  
  return(list(p = p, q = q, nary_representation = nary_representation, expansion = expansion,
              initial_part = expansion[1:(found_index - 1) %/% 1], repeating_part = expansion[found_index:(i - 1)],
              terms = bs_seen))
}

#' Convert a binary string to its rational representation
#'
#' @param binary_string A string representing a binary number, which may include a fractional part.
#' @return A list containing the floating-point approximation, numerator, and denominator of the rational number.
binary_string_to_rational <- function(binary_string) {
  # Split the binary string into integer and fractional parts
  parts <- strsplit(binary_string, "\\.")[[1]]
  int_part <- parts[1]
  frac_part <- ifelse(length(parts) > 1, parts[2], "")
  
  # Convert integer part to decimal
  int_decimal <- 0
  if (nchar(int_part) > 0) {
    int_decimal <- sum(as.numeric(strsplit(int_part, NULL)[[1]]) * 2^(rev(seq_len(nchar(int_part)) - 1)))
  }
  
  # Initialize the numerator and denominator for the rational number
  numerator <- as.bigq(int_decimal)
  denominator <- as.bigq(1)
  
  # Convert fractional part to rational
  if (nchar(frac_part) > 0) {
    for (i in 1:nchar(frac_part)) {
      bit <- as.numeric(substr(frac_part, i, i))
      numerator <- numerator * as.bigq(2) + as.bigq(bit)
      denominator <- denominator * as.bigq(2)
    }
  }
  
  # Result in decimal
  floating_point_approximation <- as.numeric(numerator) / as.numeric(denominator)
  
  return(list(floating_point_approximation = floating_point_approximation, numerator = numerator, denominator = denominator))
}