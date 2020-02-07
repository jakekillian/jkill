install.packages("devtools")
profit = function(x){(-1)*(x[1]*(600-3*x[1]+x[2])+x[2]*(800-2*x[2]+x[1])-(200*(600-3*x[1]+x[2]))-(300*(800-2*x[2]+x[1])))}
x = c(100,100)
ans = optim(x,profit)
print(ans)

p = function(c1){
  profit = function(x){(-1)*(x[1]*(600-3*x[1]+x[2])+x[2]*(800-2*x[2]+x[1])-(c1*(600-3*x[1]+x[2]))-(300*(800-2*x[2]+x[1])))}
  
  x = c(100,100)
  ans = optim(x,profit)
  return(ans)
}
p(200)
cost1 = seq(200,600,20)
ans.profit = 0
