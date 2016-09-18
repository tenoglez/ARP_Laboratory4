library(testthat)
library(linreg)

# example
data("iris")
lm.regres <- lm(iris$Sepal.Length ~ iris$Sepal.Width)
# object
obj <- linregRC(iris$Sepal.Length ~ iris$Sepal.Width, iris)

lm("iris$Sepal.Length ~ iris$Sepal.Width", iris)

# method coefficients()
test_that("Coefficients correct values", {
  expect_equal(obj$coefficients(), lm.regres$coefficients)
})

test_that("Error input ---", {
  expect_error(linregRC("a", iris), "class(formula) == \"formula\" is not TRUE", fixed=TRUE)
})

# method resid()
test_that("Residuos correct values", {
  expect_equal(obj$resid(), lm.regres$resid())
})

# method predic()
test_that("Predicts correct values", {
  expect_equal(obj$predic(), lm.regres$predic())
})



# function linreg()
expect_equal(class(x)=="linreg")
expect_equal(class(x$resid)=="numeric")
expect_equal(class(x$pred)=="numeric")
expect_equal(class(x$coef)=="numeric")


# arguments linreg
expect_equal(class(x$resid)=="formula")