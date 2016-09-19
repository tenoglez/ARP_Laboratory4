library(testthat)
library(linreg)

# example data
data("iris")
data("faithful")
lm.iris <- lm(iris$Sepal.Length ~ iris$Sepal.Width)
lm.faithful <- lm(faithful$eruptions ~ faithful$waiting)

# object
obj.iris <- linregRC(iris$Sepal.Length ~ iris$Sepal.Width, iris)
obj.faithful <- linregRC(faithful$eruptions ~ faithful$waiting, faithful)

# Error input function linregRC
test_that("Error input linregRC", {
  expect_error(linregRC("a", iris), "class(formula) == \"formula\" is not TRUE", fixed=TRUE)
  expect_error(linregRC(formula, "str"), "is.data.frame(data) is not TRUE")
})


# method coefficients()
test_that("Coefficients method", {
  expect_equal(round(obj$coefficients(), 5), round(lm.regres$coefficients, 5))
})



# method resid()
test_that("Residuos correct values", {
  expect_equal(obj$resid(), lm.regres$resid())
})


# method predic()
test_that("Predicts correct values", {
  expect_equal(obj$predic(), lm.regres$predic())
})



#  testing linreg attributes' class

expect_equal(class(obj.iris)=="linreg")
expect_true(is.numeric(obj.iris$resid))
expect_equal(class(obj.iris$pred)=="numeric")
expect_equal(class(obj.iris$coef)=="numeric")