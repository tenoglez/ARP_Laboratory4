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
#' #plot(qr.obj)
#' 

plot.linreg <- function(linreg.obj, standarized.residuals = FALSE){
  library(ggplot2)
  library(png)
  library(grid)
  library(gridExtra)
  
  df <- data.frame(linreg.obj$predicted, linreg.obj$residuals)
  ylabel.name <- "Residuals"
  
  standarized <- sqrt(abs((linreg.obj$residuals - mean(linreg.obj$residuals))/sd(linreg.obj$residuals)))
  df.stand <- data.frame(linreg.obj$predicted, standarized)
  ylabel.name.stand <- "Standarized Residuals"
  
  
  img <- readPNG(system.file("extdata", "liu_logo.png", package = "linearRegression")) 
  g <- rasterGrob(img, interpolate=TRUE) 
  
  colnames(df) <- c("predicted", "residuals")
  colnames(df.stand) <- c("predicted", "residuals")
  
  gp1 <- ggplot(df,aes(x=predicted, y=residuals)) 
  gp1 <- gp1 + theme(panel.background = element_rect(colour = "black", fill = "#B3F5F3"))
  gp1 <- gp1 + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  gp1 <- gp1 + geom_point(size = 2)
  gp1 <- gp1 + geom_smooth(method=lm,se=FALSE, color = "red")
  gp1 <- gp1 + xlab("Fitted values")
  gp1 <- gp1 + ylab(ylabel.name)
  gp1 <- gp1 + ggtitle(paste(ylabel.name, "vs Fitted"))
  
  
  gp2 <- ggplot(df.stand,aes(x=predicted, y=residuals))
  gp2 <- gp2 + theme(panel.background = element_rect(colour = "black", fill = "#B3F5F3"))
  gp2 <- gp2 + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  gp2 <- gp2 + geom_point(size = 2)
  gp2 <- gp2 + geom_smooth(method=lm,se=FALSE, color = "red")
  gp2 <- gp2 + xlab("Fitted values")
  gp2 <- gp2 + ylab(expression(sqrt("Standarized Residuals")))
  gp2 <- gp2 + ggtitle(paste(ylabel.name.stand, "vs Fitted"))
  
  grid.arrange(gp1, gp2, ncol = 1)
}