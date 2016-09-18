# unit test

# function linreg()
expect_equal(class(x)=="linreg")
expect_equal(class(x$resid)=="numeric")
expect_equal(class(x$pred)=="numeric")
expect_equal(class(x$coef)=="numeric")

# arguments linreg
expect_equal(class(x$resid)=="formula")