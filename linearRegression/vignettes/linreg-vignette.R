## ----QRlinreg, eval=FALSE------------------------------------------------
#  data(iris)
#  linreg.obj <- QRlinreg(Sepal.Length ~ Sepal.Width, iris)

## ----print.linreg, eval=FALSE--------------------------------------------
#  qr.obj <- QRlinreg(Sepal.Length ~ Sepal.Width, iris)
#  print(qr.obj)

## ----plot.linreg, eval=FALSE---------------------------------------------
#  qr.obj <- QRlinreg(Sepal.Length ~ Sepal.Width, iris)
#  plot(qr.obj)
#  plot(qr.obj, standarized_residuals = TRUE)

## ----summary.linreg, eval=FALSE------------------------------------------
#  qr.obj <- QRlinreg(Sepal.Length ~ Sepal.Width, iris)
#  summary(qr.obj)

