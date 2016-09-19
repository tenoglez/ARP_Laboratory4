linreg <- setRefClass("linreg", fields = c("indep", "dep", "coef", "predicted", "residuals", "data", "formula"))
linreg$methods(list(coefficients
               = function(){
                  indep <<- model.frame(formula)
                  dep <<- as.matrix(indep[1])
                  colnames(dep) <<- colnames(indep)[1]
                  indep <<- cbind(rep(1,length(indep)),as.matrix(indep[-1]))
                  coef <<- as.vector(solve(crossprod(indep))%*%t(indep)%*%dep)
                  names(coef) <<- c("(Intercept)", colnames(indep)[-1])
                  coef
               }
               ,pred
               = function(){
                 predicted <<- indep%*%coeficientes(formula)
                 predicted
               }
               ,resid
               = function(){
                 residuals <<- dep - predicted
                 residuals
               }
              )
)

print.linreg <- function(regression){
  regression$coef
}

linregRC <- function(formula, data){
  regression <- linreg$new(data = data, formula = formula)
  regression$coefficients()
  regression$pred()
  regression$resid()
  
  return(regression)
}

aa <- linregRC(regres, iris)
str(aa)
methods(aa)
linreg
??usingMethods
