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
  expect_error(QRlinreg("a", iris), "class(formula) == \"formula\" is not TRUE", fixed = TRUE)
  expect_error(QRlinreg(faithful$eruptions ~ faithful$waiting, "str"), "is.data.frame(data) is not TRUE", fixed = TRUE)
})

# testing expected output of our methods:
# for each one we had to round the results 

# coef() method
test_that("coef() method", {
  expect_equal(obj.iris$coefficients, lm.iris$coefficients)
  expect_equal(obj.faithful$coefficients, lm.faithful$coefficients)
})

# resid() method
test_that("Residuos correct values", {
  expect_equal(obj.iris$residuals, lm.iris$residuals)
  expect_equal(obj.faithful$residuals, lm.faithful$residuals)
})

# pred() method
test_that("pred() method", {
  expect_equal(obj.iris$predicted, lm.iris$fitted.values)
  expect_equal(obj.faithful$predicted, lm.faithful$fitted.valued)
})


#  testing linreg and attributes' class
test_that("testing classes", {
  expect_equal(class(obj.iris)[1],"linreg")
  expect_true(is.numeric(obj.iris$coefficients))
  expect_true(is.numeric(obj.iris$residuals))
  expect_true(is.numeric(obj.iris$predicted))
})
