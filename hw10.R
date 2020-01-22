# problem 6
A=0.00022
B=2.7*10^-6
C=1.124

# probabillity
mu = function(t) A+B*C^t
tpx = function (t,x){
  inner = function(k)A + B * C^k
  inner2 = Vectorize(integrate(inner,x,t+x)$value)
  return(exp(-inner2))
}

# premium related
i = 0.04
v = 1/(1+i)
exp = 0
for (k in 0:9){
  every = 0.03 * (1 - (1-tpx(k,50))^2) * v^k
  exp = exp + every
}
prem_relate = 1 - exp

# annuity
first = 0
for (k in 0:9){
  every = 100000 * 2 * tpx(k+1,50) * (tpx(k,50) - tpx(k+1,50)) * v^(k+1)
  first = every + first
}
first

last = 0
for (k in 0:9){
  every = 200000 * 2 * (1 - tpx(k,50)) * (tpx(k,50) - tpx(k+1,50)) * v^(k+1)
  last = every + last
}
last

both = 0
for (k in 0:9){
  every = 300000 * (tpx(k,50) - tpx(k+1,50))^2 * v^(k+1)
  both = every + both
}
both

prem = (first + both + last)/ prem_relate

# problem 7
V_4 = 0
for (k in 0:5){
  every = 200000 * (tpx(k,54) - tpx(k+1,54)) * v^(k+1) + 0.03 * prem * tpx(k,54)* v^k
  V_4 = every + V_4
}
V_4

tpx(3,60)

# problem 9
mu01 = 0.01
mu02 = 0.015
mu03 = 0.01
mu12 = 0.03
tpx00 = function (t,x){
  inner = Vectorize (function(k) mu01 + mu02 + mu03)
  return(exp(-integrate(inner,x,x+t)$value))
}
tpx02_12 = function (x){
  inner = Vectorize(function(k) tpx00(k,x)*mu02)
  return(integrate(inner,0,1/12)$value)
}
tpx02_12(55)

tpx01 = function(t,x){
  
}