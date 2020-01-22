A=0.00022
B=2.7*10^-6
C=1.124

# problem 1 - part 1
i1 = 0.07
i2 = 0.05

# probabillity
mu = function(t) A+B*C^t
tpx = function (t,x){
  inner = function(k)A + B * C^k
  inner2 = Vectorize(integrate(inner,x,t+x)$value)
  return(exp(-inner2))
}

# pension side
d12 = 12*(1-(1+i2)^(-1/12))
a_10guaran = (1-(1+i2)^-10)/d12
# monthly annuity
a_mon = function(t,n,x){
  all = 0
  for (k in 0: (t*n-1)){
    every = tpx(k/n,x) / (1+0.045)^(k/n)/n
    all = all+ every
  }
  return(all)
}
pen = 273367.4492*0.7
pension = (a_10guaran+tpx(10,60)/(1+i2)^10*a_mon(100,12,70))*pen

# contribution
c1 = integrate(function(t)40000*1.07^t*1.07^(35-t),0,20)$value
c2 = 40000*1.07^20*integrate(function(t)1.04^t*1.07^(15-t),0,15)$value
c_rate = pension/(c1+c2)

# problem - part 2
pen2 = 40000*1.05^34*0.05/log(1.05)
d12_2 = 12*(1-(1+0.045)^(-1/12))
a_10guaran_2 = (1-(1+0.045)^-10)/d12_2
pension2 = (a_10guaran_2+tpx(10,60)/(1+0.045)^10*a_mon(100,12,70))*pen2
c = 40000*integrate(function(t)1.05^t*1.06^(35-t),0,35)$value
r = c*c_rate/pension2

# problem 4
contri = 0
for (t in 0:39){
  every = 0.12*50000*1.05^t*1.08^(39.5-t)
  contri = contri +every
}
contri/11
