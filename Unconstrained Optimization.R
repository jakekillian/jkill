install.packages("MASS")
install.packages("NlcOptim")



Outer = function(f,x){
  n1 = length(x[[1]])
  n2 = length(x[[2]])
  res = matrix(0,nrow=n1,ncol=n2)
  rownames(res) = x[[1]]
  colnames(res) = x[[2]]
  for (i in 1:n1){
    for (j in 1:n2){
      res[i,j]=f(c(x[[1]][[i]],x[[2]][[j]]))
    }
  }
  return(res)
}

X = list(x=seq(0,6000,100),y=seq(0,9000,100))
Z = Outer(P,X)
## This feasible region is an easy one to draw ###
#contour(x=X$x,y=X$y,z=-Z,lwd=3)
#abline(h=8000,col="red",lwd=3) #line at y=8000
#abline(v=5000,col="red",lwd=3) #line at x=5000
#abline(h=0,col="red",lwd=3) #line at y=0
#abline(v=0,col="red",lwd=3) #line at x=0
#abline(a=10000,b=-1,col="red",lwd=3) #line with y=bx+a; b is the slope, a is the y-intercept
#pts = list(x=c(0,0,2000,5000,5000),y=c(0,8000,8000,5000,0)) #we need the intersection points for shading
#polygon(pts$x,pts$y,density = 20,col="red") #this shades the feasible region red
##########
obj = function(x){(10*x[1]^(0.6)*x[2]^(0.4))*(-1)}
### Contour Plot
X = list(x=seq(0,20,0.1),y=seq(0,20,0.1))
Z = Outer(obj,X)
contour(x=X$X,y=X$y, z=-Z,lwd=2)
abline(a=10,b=-5/3, col="red",lwd=2)

contour(x=X$X,y=X$y, z=-Z,lwd=2,
        levels=c(20,37.55,60,80,100,120,140,160,180))
abline(a=10,b=-5/3, col="red",lwd=2)

x0 = c(3,5)
Aeq = matrix(c(50,30),nrow=1)
Beq = matrix(300)
ans = solnl(x0, obj, Aeq=Aeq, Beq=Beq)
print(ans)
### solnl ###
solnl(X = NULL, objfun = NULL, confun = NULL, A = NULL, B = NULL,
      Aeq = NULL, Beq = NULL, lb = NULL, ub = NULL, tolX = 1e-05,
      tolFun = 1e-06, tolCon = 1e-06, maxnFun = 1e+07, maxIter = 4000)

#### Number 1 ###
library(MASS);library(NlcOptim)
obj = function(x){(1/2*(x[2]-32)*x[1]^2)*(-1)}
con = function(x){
  f = NULL
  f = rbind(f,x[2]^2*x[1]-10000)
  return(list(ceq=f,c=NULL))
}
x0 = c(1,100)
solnl(x0,objfun = obj, confun = con)