#' Class of linear regression models using de OLS methods
#' 
#' @description 
#' Class of linear regression models using de OLS method.
#' 
#' 
#' @param graph A \code{data.frame} with three columns that describes the graph: \code{v1} (nodes where edges start), \code{v2} (nodes where edges end) and \code{w} (weigth of the edges)
#' @param init_node A positive scalar number for the initial node
#' 
#' @method \code{coef} :returns the vector of residuals \code{e}.
#' @method \code{resid} returns the predicted values \code{y}.
#' @method \code{pred}: return the coefficients as a named vector.
#' 
#' @references 
#' Ordinary Least Squares - \url{https://en.wikipedia.org/wiki/Ordinary_least_squares}
#' 
#' @examples
#' OLSlinreg(iris$Sepal.Length ~ iris$Sepal.Width, iris)
#' OLSlinreg(faithful$eurption ~ faithful$waiting, faithful)

# linreg <- setRefClass("linreg", fields = c("indep", "dep", "coefficients", "predicted", "residuals", "data", "formula"))
# linreg$methods(list(coef
#                = function(){
#                   indep <<- model.frame(formula)
#                   dep <<- as.matrix(indep[1])
#                   colnames(dep) <<- colnames(indep)[1]
#                   indep <<- cbind(rep(1,length(indep)),as.matrix(indep[-1]))
#                   coefficients <<- as.vector(solve(crossprod(indep))%*%t(indep)%*%dep)
#                   names(coefficients) <<- c("(Intercept)", colnames(indep)[-1])
#                   coefficients
#                }
#                ,pred
#                = function(){
#                  predicted <<- indep%*%coefficients
#                  predicted
#                }
#                ,resid
#                = function(){
#                  residuals <<- dep - predicted
#                  residuals
#                }
#               )
# )
# 
# print.linreg <- function(regression){
#   regression$coef
# }
# 
# OLSlinreg <- function(formula, data){
#   regression <- linreg$new(data = data, formula = formula)
#   regression$coef()
#   regression$pred()
#   regression$resid()
#   
#   return(regression)
# }