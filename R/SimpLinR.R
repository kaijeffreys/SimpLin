#' Simple Linear Regression
#'
#' This function takes in two numeric vectors, the predictor variable and the
#' the response variable, and runs a simple linear regression
#'
#' @param x Numeric vector containing the predictor variable
#' @param y Numeric vector containing the response variable
#' @return A list containing five separate elements: a vector of coefficients, a vector of their standard errors, a matrix of the confidence intervals of the coefficients, a vector of the residuals, and a vector of the fitted values
#'
#' @export

SimpLinR <- function(x, y) {
  # Check for errors on input
  if(!is.numeric(x) | !is.numeric(y)) {
    stop("Input error: input vectors are not numeric")
  }

  if(length(x) != length(y)) {
    stop("Input error: x and y are not the same length")
  }

  if(length(x) < 3) {
    warning("Vector does not have enough elements for inference")
    if(length(x) < 2) {
      stop("Input error: x and y need at least three elements for full use of function")
    }
  }
  
  # Run the function return the output
  # out <- .Call("_SimpLin_SimpLinCpp.cpp", x, y, SimpLinCpp)
  out <- SimpLinCpp(x,y)
  return(out)
}

