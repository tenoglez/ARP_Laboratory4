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
  quants <- quantile(linreg.obj$residuals, probs = seq(0, 1, 0.25))
  std.error <- sqrt(aa$variance)
  t.val <- coefficients/sqrt(aa$variance)
  
  df <- data.frame(linreg.obj$coefficients, std.error, t.val)
  colnames(df) <- c("Estimate", "Std. Error", "t-value" )
  rownames(df) <- paste("(Intercept)", rownames(df))
  
  list(Residuals=quants, Coefficients=df)
}