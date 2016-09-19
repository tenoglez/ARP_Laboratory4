class(aa$indep)
Q <- aa$indep[,1:3]/sqrt(sum(aa$indep[,1:3]^2))
R<-t(Q)%*%aa$indep
ff <-solve(t(R)%*%R)%*%t(R)%*%t(Q)%*%aa$dep 
ss<-lm(regres)
Q%*%t(Q)
round((ss$coefficients),6) == round(ff,6)