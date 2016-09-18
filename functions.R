linreg <- setRefClass("RCclass", fields = c("indep", "dep", "predicted", "residuals", "data", "formula"))
linreg$methods(list(coefficients
               = function(){
                  indep <<- model.frame(formula)
                  dep <<- as.matrix(indep[1])
                  colnames(dep) <<- colnames(indep)[1]
                  indep <<- cbind(rep(1,length(indep)),as.matrix(indep[-1]))
                  solve(crossprod(indep))%*%t(indep)%*%dep
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
              ))
linregRC <- function(formula, data){
  regresion <- linreg$new(data = data, formula = formula)
  regresion$coefficients()
  regresion$pred()
  regresion$resid()
  regresion
}
aa <- linregRC(regres, iris)
aa$data
aa$pred()
class(aa)
print(aa)
