profit = function (x) {return((1500-100*x)*(1+.15*x))}
x = seq(0,15,1)
plot(x,profit(x),type='o')

fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}
bisection = function(f,a,b,tol=0.0001){
  if (f(a)*f(b) > 0){
    return ("Boundary Conditions Not Met")
  }
  else{
    middle = a
    while (abs(f(middle))>tol){
      middle = (a+b)/2
      if (f(middle)*f(a)>0) (a = middle)
      else (b = middle)
      x=middle
      y=f(middle)
      ## if you want to "see" what happens at every step, take off the # of the next line ##
      #cat(sprintf("x-Val: %.4f ; f(x-val): %.4f\n",x,y))
    }
    return (middle)
  }
}

dProfit = function(x){fprime(profit,x)}
bisection(dProfit,0,20)
plot(x,profit(x),"l")
