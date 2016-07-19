#' fd_bin_size
#'
#' little helper function to calculate hist bin size using the Freedman Diaconis Rule https://en.wikipedia.org/wiki/Freedman%E2%80%93Diaconis_rule
#'
#' @param x a numeric vector
#'
#' @return the appropriate bin size according to the FD rule
#' @export
#'
fd_bin_size <- function(x){

  numerator <- (IQR(x, na.rm = TRUE, type = 7))

  denominator <- (length(x)^(1/3))

  temp_frac <- (numerator / denominator)

  2 * temp_frac

}
