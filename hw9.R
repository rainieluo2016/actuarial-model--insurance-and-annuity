# problem 8
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
tpx(1,45)
tpx(1,46)
tpx(1,74)
tpx(1/12,55+29/12)
