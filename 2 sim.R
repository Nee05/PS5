rm(list=ls())

library(sandwich)
library(lmtest)
library(ggplot2)

setwd("~/Econ/03 PhD in IRE/2nd in 1/Econometrics II/Ass/problem set 5")

beta = matrix(NA, 500, 3)

for (k in 1:500) {

#generate state level unemp mean
state <- rnorm(50, 6, 2)

#generate state level unemp sds
stva <- abs(rnorm(50, 1, 1/2))

# Setting the initial level of unemployment for each county in 1980 such that every state
#is a cluster of correlated counties
A <- matrix(cbind(rep(1980, 20), seq(1:20), rep(paste(1),20), rnorm(20, state[1], stva[1])), 20, 4)
for (i in 2:50){
  unemp0 <- rnorm(20, state[i], stva[i])
  state_name <- rep(paste(i),20)
  county_name = seq((20*(i-1)+1),(20*i))
  c <- cbind("year"=rep(1980, 20), county_name, state_name,unemp0)
  A <- rbind(A,c)
}

#Transforming the cross-sectional in a panel
for(i in 1981:2010){
  B = cbind(rep(i,1000), seq(1:1000), A[1:1000,3], A[1:1000,4])
  A=rbind(A,B)
}

A = as.data.frame(A)
A$year = as.numeric(A$year)
A$unemp0 = as.numeric(A$unemp0)
A$state_name = as.numeric(A$state_name)
A$county_name = as.numeric(A$county_name)

#Defining the treatment groups and the treatment periods
A$G1 = ifelse(A$state_name<=15 , 1, 0)
A$G2 = ifelse(A$state_name>15&A$state_name<=30, 1, 0)
A$T1 = ifelse(A$year>=1990, 1, 0)
A$T2 = ifelse(A$year>=2005, 1, 0)

A$D_early = A$G1*A$T1
A$D_late = A$G2*A$T2
A$D = ifelse(A$G1*A$T1==1|A$G2*A$T2==1, 1,0)

#Defining the Y for each model
A$unemp1=A$unemp0+A$D_early*5+A$D_late*5
A$unemp2=A$unemp0+A$D_early*2.5+A$D_late*7.5
A$unemp3=A$unemp0+A$D_early*(A$year-1989)+A$D_late*(A$year-2004)

A$state_name = as.factor(A$state_name)

reg1 = lm(unemp1~D, data = A)
reg2 = lm(unemp2~D, data = A)
reg3 = lm(unemp3~D*year, data = A)

beta[k,1]=coef(reg1)[2]
beta[k,2]=coef(reg2)[2]
beta[k,3]=coef(reg3)[3]
}

reg1 = lm(unemp1~D+state_name+as.factor(year), data = A)
reg2 = lm(unemp2~D+state_name+as.factor(year), data = A)
reg3 = lm(unemp3~D*year+as.factor(year)+state_name, data = A)

b = as.vector(3)
b[1]=coef(reg1)[2]
b[2]=coef(reg2)[2]
b[3]=coef(reg3)[83]

beta = as.data.frame(beta)

ggplot(beta, aes(x=V1))+
  geom_histogram(fill="white", color="black")+
  geom_vline(xintercept = b[1], size=1, color="red")+
  theme_classic()+
  labs(x="Distribution of the beta", y="Frequency", title = "First specification")

ggplot(beta, aes(x=V2))+
  geom_histogram(fill="white", color="black")+
  geom_vline(xintercept = b[2], size=1, color="red")+
  theme_classic()+
  labs(x="Distribution of the beta", y="Frequency", title = "Second specification")

ggplot(beta, aes(x=V3))+
  geom_histogram(fill="white", color="black")+
  geom_vline(xintercept = b[3], size=1, color="red")+
  theme_classic()+
  labs(x="Distribution of the beta", y="Frequency", title = "Third specification")



