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
  library(png)
  library(grid)
  if (standarized.residuals == TRUE){
    standarized <- sqrt(abs((linreg.obj$residuals - mean(linreg.obj$residuals))/sd(linreg.obj$residuals)))
    df <- data.frame(linreg.obj$predicted, standarized)
    ylabel.name <- "Standarized Residuals"
  }else{
    df <- data.frame(linreg.obj$predicted, linreg.obj$residuals)
    ylabel.name <- "Residuals"
  }
  
  img <- readPNG("images/liu_logo.png") 
  g <- rasterGrob(img, interpolate=TRUE) 
  
  colnames(df) <- c("predicted", "residuals")
  
  gp <- ggplot(df,aes(x=predicted, y=residuals))
  gp <- gp + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  gp <- gp + theme(panel.background = element_rect(colour = "black", fill = "#B3F5F3"))
  gp <- gp + geom_point(size = 2)
  gp <- gp + geom_smooth(method=lm,se=FALSE, color = "red")
  gp <- gp + xlab("Fitted values")
  gp <- gp + ylab(ylabel.name)
  gp <- gp + ggtitle(paste(ylabel.name, "vs Fitted"))
  gp
}