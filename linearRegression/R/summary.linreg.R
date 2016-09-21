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
  std.error <- sqrt(linreg.obj$variance)
  t.val <- coefficients/sqrt(linreg.obj$variance)
  p.val <- 2*pt(1-abs(t.val), linreg.obj$degrees_freedom)
  
  df <- data.frame(linreg.obj$coefficients, std.error, t.val, p.val)
  colnames(df) <- c("Estimate", "Std. Error", "t-value", "Pr(>|t|)" )
  rownames(df)[1] <- "(Intercept)"
  
  list(Residuals=quants, Coefficients=df)
}
summary(aa)
