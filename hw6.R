A=0.00022
B=2.7*10^-6
C=1.124
i = 0.05

# probability
mu = function(t) A+B*C^t
tpx = function (t,x){
  inner = function(k)A + B * C^k
  inner2 = Vectorize(integrate(inner,x,t+x)$value)
  return(exp(-inner2))
}
tpx(10,30)

tp3040 = function(t){
  return(tpx(t,30)*tpx(t,40))
}
tp3040(10)

#
tq30_1 = function(t){
  inner = function(k) tp3040(k)*mu(30+k)
  return(integrate(Vectorize(inner),0,t)$value)
}
tq30_1(10)

# annuity
a_first = 0
for (k in 0:500){
  every = tpx(k,25)*tpx(k,30)/(1+i)^k
  a_first = a_first + every
}
a_first

a_25 = 0
for (k in 0:500){
  every = tpx(k,25)/(1+i)^k
  a_25 = a_25 + every
}
a_25

a_30 = 0
for (k in 0:500){
  every = tpx(k,30)/(1+i)^k
  a_30 = a_30 + every
}
a_30

a_last = a_25 + a_30 - a_first
a_last

a_rever = 0
for (k in 0:500){
  every = (1 - tpx(k,25))*tpx(k,30)/(1+i)^k
  a_rever = a_rever + every
}
a_rever

a_first+a_last+10*a_rever

# insurance
tp2530 = function(t){
  return(tpx(t,30)*tpx(t,25))
}

A_first = function(t){
  inner = function(k) tpx(k,25)*tpx(k,30)*(mu(k+25)+mu(k+30))/(1+i)^k
  return(integrate(Vectorize(inner),0,t)$value)
}
A_first(300)

A_25first = function(t){
  inner = function(k) tpx(k,25)*tpx(k,30)*mu(k+25)/(1+i)^k
  return(integrate(Vectorize(inner),0,t)$value)
}
A_25first(10)

A_30second = function(t){
  inner = function(k) (1-tpx(k,25))*tpx(k,30)*mu(k+30)/(1+i)^k
  return(integrate(Vectorize(inner),0,t)$value)
}
A_30second(200)

A_first(300)*100 + 500*(A_25first(10)+A_30second(200))

# question 6
a_6070 = 0
for (k in 0:500){
  every = tpx(k,60)*tpx(k,70)/(1+i)^k
  a_6070 = a_6070 + every
}

a_70 = 0
for (k in 0:500){
  every = tpx(k,70)/(1+i)^k
  a_70 = a_70 + every
}

a_60 = 0
for (k in 0:500){
  every = tpx(k,60)/(1+i)^k
  a_60 = a_60 + every
}


y6=20000*(a_60 + a_70 - a_6070-1)
y6

a_6070_10 = 0
for (k in 0:9){
  every = tpx(k,60)*tpx(k,70)/(1+i)^k
  a_6070_10 = a_6070_10 + every
}
z6 = 30000*a_6070_10
z6

#problem 7
a_rever60 = 0
for (k in 0:500){
  every = tpx(k,60)*(1-tpx(k,70))/(1+i)^k
  a_rever60 = a_rever60 + every
}
25000*a_rever60

#problem 8
a8=0
b8=0.0003
c8=1.075
tpx8 = function (t,x){
  inner = function(k)a8 + b8 * c8^k
  inner2 = Vectorize(integrate(inner,x,t+x)$value)
  return(exp(-inner2))
}
tpx8(0,25)
a8_25 = 0
for (k in 0:500){
  every = tpx8(k,25)/(1+i)^k
  a8_25 = a8_25 + every
}
a8_25

A_msecond = 0
for (k in 0:500){
  every = (1-tpx8(k,25))*(tpx8(k,25)-tpx8(k+1,25))/((1+i)^(k+1))
  A_msecond = A_msecond + every
}
A_msecond

a8_first = 0
for (k in 0:500){
  every = tpx8(k,25)^2/(1+i)^k
  a8_first = a8_first +every
}
a8_first

prem8 = 100000*A_msecond/a8_25
prem8


prem8_c = 100000*(1-a8_25*i/(1+i)-(1-a8_first*i/(1+i))/2)/a8_25
prem8_c

1-a8_25*i/(1+i)
i

# problem 8_2
a8_35 = 0
for (k in 0:500){
  every = tpx8(k,35)/(1+i)^k
  a8_35 = a8_35 + every
}
a8_35

1-a8_35*i/(1+i)

A8_35 = 0
for (k in 0:500){
  every = (tpx8(k,35)-tpx8(k+1,35))/(1+i)^(k+1)
  A8_35 = A8_35 + every
}
A8_35

V10 = 100000*A8_35-prem8_c*a8_35 
V10

#problem 8 part 3
a8_35first = 0
for (k in 0:500){
  every = tpx8(k,35)^2/(1+i)^k
  a8_35first = a8_35first + every
}
a8_35first

A8_35msecond = 0.5 + i/(1+i)*(0.5*a8_35first - a8_35)
V10_both = 100000*A8_35msecond - prem8_c*a8_35



# problem 9
## setting
a9x = 0.0001
b9x = 0.0004
c9x = 1.075
a9y = 0.0001
b9y = 0.0003
c9y = 1.07
mu9x = function(t) a9x + b9x*c9x^t
mu9y = function(t) a9y + b9y*c9y^t
tp9x = function(t,x){
  inner = Vectorize(integrate(mu9x,x,t+x)$value)
  return(exp(-inner))
}
tp9y = function(t,x){
  inner = Vectorize(integrate(mu9y,x,t+x)$value)
  return(exp(-inner))
}
#insurance
A9xy = function(x,y,t){
  inner = Vectorize(function(k) tp9x(k,x)*tp9y(k,y)*(mu9x(k+x)+mu9y(k+y))/(1+i)^k)
  return(integrate(inner,0,t)$value)
}
A9xy(28,24,200)
#annuity
a9xy_25m = 0
for (k in 0:299){
  every = tp9x(k/12,28)*tp9y(k/12,24)/(1+i)^(k/12)
  a9xy_25m = a9xy_25m +every
}
a9xy_25m
#premium
prem9 = (100000*A9xy(28,24,300)+250)/(0.97*a9xy_25m)
prem9
