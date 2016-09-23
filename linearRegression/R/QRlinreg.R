#' QR decomposition method on Linear Regression Class
#' @export
#' 
#' @description 
#' Creates an object of class \code{linreg} with all the statistics computed for an specific \code{formula}
#'  and the given \code{data}
#' 
#' @param formula A formula with the independent variable and dependent variable/s
#' @param data A dataset
#' 
#' @return An object of class \code{linreg}
#' 
#' @references 
#' QR Decomposition - \url{https://en.wikipedia.org/wiki/QR_decomposition}
#' 
#' @examples
#' QRlinreg(iris$Sepal.Length ~ iris$Sepal.Width, iris)
#' QRlinreg(faithful$eurption ~ faithful$waiting, faithful)
#' 

QRlinreg <- function(formula, data){
  regression <- linreg$new(data = data, formula = formula)
  regression$coef()
  regression$pred()
  regression$resid()
  regression$df()
  regression$var()
  
  return(regression)
}