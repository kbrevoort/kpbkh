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

#' @export
pct <- function(x, decimals = 1) {
  sprintf('%s percent',
          round(x*100, digits = decimals))
}

`%notin%` <- Negate(`%in%`)

#' Round data.table Columns for Displaying
#'
#' This function replaces the numbers in a data.table with rounded versions that
#' are limited to a specified number of decimal places.  The changes are made
#' by reference.
#' @param var_list A vector of variable names (as characters)
#' @param dt Data.table to be changed
#' @param decimals The number of decimals to display
#' @import data.table
#' @export
round4display <- function(dt, var_list, decimals = 0) {
  if (any(var_list %notin% names(dt)))
    stop('Not all variables in var_list are part of dt')

  for (var in var_list) {
    my_x <- dt[, var, with = FALSE]
    set(dt, j = var, value = round(my_x, decimals))
  }
  dt
}

#' Convert a number to a ratio
#'
#' This function takes a percentage and converts it to a 1-in-X ratio.  It also
#' attempts to add a qualifier (e.g., about, almost).
onein <- function(x, qualifier = TRUE) {
  if (!is.numeric(x) | length(x) != 1) stop('Invalid input to onein')
  if (x <= 0) {
    warning('Negative value sent to onein')
    return('NA')
  }

  # x may be a percentage
  if (x > 1) {
    x <- x / 100
    if (x > 1)
      stop(sprintf('Implausibly large value sent to onein: %s', x * 100))
  }

  x_inv <- 1 / x
  x_round <- round(x_inv, digits = 0)

  qual <- ''
  if (abs(x_round - x_inv) < 0.1) {
    if (qualifier) qual <- 'about '
    return(sprintf('%s1-in-%d', qual, x_round))
  } else if (x_inv > x_round) {
    if (qualifier) qual <- 'almost '
    return(sprintf('%s1-in-%d', qual, x_round))
  } else if (x_inv < x_round) {
    if (qualifier) qual <- 'more than '
    return(sprintf('%s1-in-%d', qual, x_round))
  }

  return('NA')
}
