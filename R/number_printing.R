#' Display a number in Dollars
#'
#' This function accepts a single number and converts it into dollars for display in
#' the monitoring reports.
#' @param x The numeric value to be conferted (must be length 1)
#' @param use_cents Should 2 digits to the right of the decimal be used (default = FALSE)
#' @param digits Specify the number of digits to use (includes left and right of decimal)
#' @param decimals Specifies the number of digits to the right of the decimal
#' @export
num <- function(x, use_cents = FALSE, digits, decimals) {
  if (!length(x) == 1) stop('num only accepts single numbers')
  if (!is.numeric(x)) stop('Non-numeric argument supplied to num.')
  if (is.na(x)) stop('NA value supplied to num')

  num_digits <- ceiling(log10(x))

  # Convert to appropriate level
  if (num_digits >= 12) {
    level <- 'trillion'
    x <- x / 1e12
    num_digits <- num_digits - 12
  } else if (num_digits >= 9) {
    level <- 'billion'
    x <- x / 1e9
    num_digits <- num_digits - 9
  } else if (num_digits >= 6) {
    level <- 'million'
    x <- x / 1e6
    num_digits <- num_digits - 6
  } else {
    level <- ''
  }

  if (use_cents) {
    nsmall <- 2
  } else if (!missing(digits)) {
    nsmall <- ifelse(digits > num_digits, digits - num_digits, 0)
  } else if (!missing(decimals)) {
    nsmall <- decimals
  } else if (num_digits > 0) {
    nsmall <- 0
  } else {
    nsmall <- 3
  }

  sprintf('%s %s',
          format(round(x, digits = nsmall),
                 scientific = FALSE,
                 big.mark = ',',
                 nsmall = nsmall,
                 trim = TRUE),
          level) %>%
    stringr::str_trim()
}

#' @export
dol <- function(x, ...) {
  sprintf('$%s',
          num(x, ...))
}

pct <- function(x, decimals = 1) {
  sprintf('%s percent',
          round(x*100, digits = decimals))
}
