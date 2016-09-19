linreg <- setRefClass("linreg", fields = c("indep", "dep", "coefficients", "predicted", "residuals", "data", "formula"))
linreg$methods(list(coef
                    = function(){
                      indep <<- model.frame(formula)
                      dep <<- as.matrix(indep[1])
                      colnames(dep) <<- colnames(indep)[1]
                      indep <<- cbind(rep(1,length(indep)),as.matrix(indep[-1]))
                      
                      Q <- indep[,1:dim(indep)[2]]/sqrt(sum(indep[,1:dim(indep)[2]]^2))
                      R <- t(Q)%*%indep
                      coefficients <<- solve(t(R)%*%R)%*%t(R)%*%t(Q)%*%dep 
                      
                      #coefficients <<- as.vector(solve(crossprod(indep))%*%t(indep)%*%dep)
                      names(coefficients) <<- c("(Intercept)", colnames(indep)[-1])
                      coefficients
                    }
                    ,pred
                    = function(){
                      predicted <<- indep%*%coefficients
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

QRlinreg <- function(formula, data){
  regression <- linreg$new(data = data, formula = formula)
  regression$coef()
  regression$pred()
  regression$resid()
  
  return(regression)
}