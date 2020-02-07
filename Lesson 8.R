library(MASS);library(NlcOptim)
x = c(5.49,1.33,42.67)
obj=function(x){
  
  return((-16*x[2]^2+(x[3]-32)*x[1]*x[2]+1/2*(x[3]-32)*x[1]^2)*(-1)) # Problem 1b
  
}
con = function(x){
  f = NULL
  f = rbind(f,x[3]^2*x[1]-10000)
  f = rbind(f,-32*x[2]+(x[3]-32)*x[1])
  return(list(ceq=f,c=NULL))
}
solnl(x,objfun = obj,confun = con)

####################################
library(MASS);library(NlcOptim)
x0 = c(1,1)
optim(x,obj)
x = list(x = seq(100,500), y=seq(100,500))
z = Outer(obj,x)
obj = function(x){
  return((600-3*x[1]+x[2])*x[1]+(800-2*x[2]+x[1]*x[2]))
  }
con = function(x){
  f = NULL
  f = rbind((x[1]-300)^2+(x[2]-300)^2-200^2)
  return(list(ceq = f, c = NULL))
}
solnl(x, objfun = obj, confun = con)

parametric
####################################


