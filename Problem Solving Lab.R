library(MASS); library(NlcOptim)
obj = function(x){
  x[1]^2+x[2]^2
}
con = function(x){
  f= NULL
  f=rbind(f,x[1]+2*x[2]-5)
  f=rbind(x[1]>0)
  f=rbind(x[2]>0)
  return(list(ceq=f, c=NULL))
}
solnl(x, objfun = obj, confun = con)

######################
library(MASS); library(NlcOptim)
obj = function(x){
  return(x[1]^2+x[2]^2)
}
x = c(1,1)
ans = optim(x,obj,method = "BFGS")
print(ans)
con = function(x){
  f = NULL
  f = rbind(f,x[1]+2*x[2]-5)
  f=rbind(x[1]>=0)
  f=rbind(x[2]>=0)
  return(list(ceq=f, c=NULL))
}
solnl(x,objfun = obj,confun = con)
############################# NUMBER 2
obj = function(x){
  return((-2*x[1]^2-x[2]^2+x[1]*x[2]+8*x[1]+3*x[2])*(-1))
}
x = c(1,1)
ans = optim(x,obj)
print(ans)
############NUMBER 1D###############
f =function(x){(x[1]^2+x[2]^2+x[3]^2)}
x0 = c(1,1,1)
Aeq = matrix(c(1,0,2,1,1,0),nrow=2,byrow=TRUE)
Beq = (c(6,12))
A = matrix(c(-1,0,0,0,-1,0,0,0-1),nrow = 3,byrow = T)
B = matrix(c(0,0,0))
solnl(x0,f,Aeq=Aeq,Beq=Beq,A=A,B=B)


