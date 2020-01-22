A=0.00022
B=2.7*10^-6
C=1.124
i = 0.06

# probabillity
mu = function(t) A+B*C^t
tpx = function (t,x){
  inner = function(k)A + B * C^k
  inner2 = Vectorize(integrate(inner,x,t+x)$value)
  return(exp(-inner2))
}

# monthly annuity
a_mon = function(t,n,x){
  all = 0
  for (k in 0: (t*n-1)){
    every = tpx(k/n,x) / (1+i)^(k/n)/n
    all = all+ every
  }
  return(all)
}

a_mon(100,12,55)
a_mon(100,12,65)
y = (1-a_mon(100,12,65)/(1+i)^10/a_mon(100,12,55))/120
z = (1-a_mon(100,12,65)/(1+i)^5/a_mon(100,12,60))/60
