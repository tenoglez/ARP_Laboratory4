#' summary() method of linreg class
#' @export
#' 
#' @description 
#' Method that prints out the a summary of an object from the class linreg
#' 
#' @param linreg.obj An object of linreg class
#' 
#' @references 
#' summary() manual - \url{https://stat.ethz.ch/R-manual/R-devel/library/base/html/summary.html}
#' 
#' @examples
#' qr.obj <- QRlinreg(iris$Sepal.Length ~ iris$Sepal.Width, iris)
#' summary(qr.obj)
#' 

summary.linreg <- function(linreg.obj){
  linreg.obj$coef
}
?quantile
quants <- quantile(aa$residuals, probs = seq(0, 1, 0.25))
zz <-summary(lm(regres,iris))
