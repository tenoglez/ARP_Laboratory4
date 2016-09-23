#' Class of linear regression models using QR decompoisiton method
#' @export
#' 
#' @description 
#' Class of linear regression models using QR decompoisiton method
#' 
#' @section Attributes:
#' \itemize{
#'      \item{\code{coefficients}: vector of coefficients}
#'      \item{\code{predicted}: vector of predicted values}
#'      \item{\code{residuals}: vector of residuals}
#'    }
#' 
#' @section Methods:
#' \itemize{
#'      \item{\code{coef}: returns the vector of residuals \code{e}.}
#'      \item{\code{resid}: returns the predicted values \code{y}.}
#'      \item{\code{pred}: return the coefficients as a named vector.}
#'    }
#' 
#' @references 
#' QR Decomposition - \url{https://en.wikipedia.org/wiki/QR_decomposition}
#' 
#' @examples
#' linreg$new(data = iris, formula = iris$Sepal.Length ~ iris$Sepal.Width)
#' linreg$new(data = faithful, formula = faithful$eruption ~ faithful$waiting)


linreg <- setRefClass("linreg", fields = c("indep", "dep", "coefficients", "predicted", "residuals", "variance", "degrees_freedom", "data", "formula"))
linreg$methods(list(coef = function(){
                      stopifnot(class(formula) == "formula", is.data.frame(data))

                      indep <<- model.frame(formula, data=data)
                      dep <<- as.matrix(indep[1])
                      colnames(dep) <<- colnames(indep)[1]
                      indep <<- cbind(rep(1,length(indep)),as.matrix(indep[-1]))
                      
                      Q <- indep[,1:dim(indep)[2]]/sqrt(sum(indep[,1:dim(indep)[2]]^2))
                      R <- t(Q)%*%indep
                      coefficients <<- solve(t(R)%*%R)%*%t(R)%*%t(Q)%*%dep 
                      
                      #coefficients <<- as.vector(solve(crossprod(indep))%*%t(indep)%*%dep)
                      names(coefficients) <<- c("(Intercept)", colnames(indep)[-1])
                      coefficients
                    },
                    pred = function(){
                      predicted <<- indep%*%coefficients
                      predicted
                    },
                    resid = function(){
                      residuals <<- dep - predicted
                      residuals
                    },
                    df = function() {
                      degrees_freedom <<- nrow(indep) - (ncol(indep))
                    },
                    var = function() {
                      v.res <- as.numeric((t(residuals)%*%residuals)) / degrees_freedom
                      variance <<- matrix(as.numeric((diag(ncol(indep))*v.res) * solve(t(indep)%*%indep)), nrow=3)
                      variance <<- diag(variance)
                    }
                )
            )