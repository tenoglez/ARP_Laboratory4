# unittest
library(testthat)
library(linearRegression)


# testing with dataset "iris" and "faithful"
data("iris")
data("faithful")
# creating linear regression with lm function
lm.iris <- lm(iris$Sepal.Length ~ iris$Sepal.Width)
lm.faithful <- lm(faithful$eruptions ~ faithful$waiting)

# example objects to calculate linear regresion (using QR)
obj.iris <- QRlinreg(iris$Sepal.Length ~ iris$Sepal.Width, iris)
obj.faithful <- QRlinreg(faithful$eruptions ~ faithful$waiting, faithful)

# Input error for function QRlinreg
test_that("Input error for function QRlinreg: ", {
  expect_error(QRlinreg("a", iris), "class(formula) == \"formula\" is not TRUE", fixed=TRUE)
  expect_error(QRlinreg(formula, "str"), "is.data.frame(data) is not TRUE")
})

# testing expected output of our methods:
# for each one we had to round the results 

# coef() method
test_that("coef() method", {
  expect_equal(round(obj.iris$coefficients, 5), round(lm.iris$coefficients, 5))
  expect_equal(round(obj.faithful$coefficients, 5), round(lm.faithful$coefficients, 5))
})

# resid() method
test_that("Residuos correct values", {
  expect_equal(round(obj.iris$residuals, 5), round(lm.iris$residuals, 5))
  expect_equal(round(obj.faithful$residuals,5), round(lm.faithful$residuals, 5))
})

# pred() method
test_that("pred() method", {
  expect_equal(round(obj.iris$predicted, 5), round(lm.iris$fitted.values, 5))
  expect_equal(round(obj.faithful$predicted, 5), round(lm.faithful$fitted.valued, 5))
})


#  testing linreg and attributes' class
expect_equal(class(obj.iris) == "linreg")
expect_true(is.numeric(obj.iris$coefficients))
expect_true(is.numeric(obj.iris$residuals))
expect_true(is.numeric(obj.iris$predicted))
