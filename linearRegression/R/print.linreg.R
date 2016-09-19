#' print() method of linreg class
#' @export
#' 
#' @description 
#' Method that prints out the coefficients and coefficient names, similar as done by the \code{lm} class.
#' 
#' @param linreg.obj An object of linreg class
#' 
#' @references 
#' print() manual - \url{https://stat.ethz.ch/R-manual/R-devel/library/base/html/print.html}
#' 
#' @examples
#' qr.obj <- QRlinreg(iris$Sepal.Length ~ iris$Sepal.Width, iris)
#' print(qr.obj)
#' 

print.linreg <- function(linreg.obj){
  linreg.obj$coef
}