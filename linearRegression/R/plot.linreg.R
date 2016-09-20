#' plot() method of linreg class
#' @export
#' 
#' @description 
#' Method that plot Fitted Values vs Residuals
#' 
#' @param linreg.obj An object of linreg class
#' 
#' @references 
#' plot() manual - \url{https://stat.ethz.ch/R-manual/R-devel/library/base/html/plot.html}
#' 
#' @examples
#' qr.obj <- QRlinreg(iris$Sepal.Length ~ iris$Sepal.Width, iris)
#' plot(qr.obj)
#' 

plot.linreg <- function(linreg.obj, standarized.residuals = FALSE){
  library(ggplot2)
  if (standarized.residuals == TRUE){
    standarized <- sqrt(abs((linreg.obj$residuals - mean(linreg.obj$residuals))/sd(linreg.obj$residuals)))
    df <- data.frame(linreg.obj$predicted, standarized)
    ylabel.name <- "Standarized Residuals"
  }else{
    df <- data.frame(linreg.obj$predicted, linreg.obj$residuals)
    ylabel.name <- "Residuals"
  }
  
  colnames(df) <- c("predicted", "residuals")
  
  gp <- ggplot(df,aes(x=predicted, y=residuals))
  gp <- gp + geom_point()+theme_bw()
  gp <- gp + geom_smooth(method=lm,se=FALSE)
  gp <- gp + xlab("Fitted values")
  gp <- gp + ylab(ylabel.name)
  gp <- gp + ggtitle("Residuals vs Fitted")
  gp
}