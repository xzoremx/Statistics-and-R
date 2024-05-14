set.seed(1)

a = rnorm(5)
sd(a)

t_stat = sqrt(5)*mean(a)/sd(a)
t_stat

#------


t_statgen = function(n){
  a = rnorm(n)
  t_stat = sqrt(5)*mean(a)/sd(a)
  return(t_stat)
  
}


B0 = 1000
t_statistics = replicate(B0, t_statgen(5))
t_statistics

mean(t_statistics >= 2)



#-------------------

library(rafalib)
mypar(3,2)

Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B,{X <- rnorm(N) sqrt(N)*mean(X)/sd(X)})
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=N-1),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 


#--------------



ttest_gen = function(n, mean = 0, sd = 1){
  
  a1 = rnorm(n, mean, sd)
  a2 = rnorm(n, mean, sd)
  tstat <- t.test(a1, a2, var.equal = TRUE)$statistic
  return(tstat)  
  
}

B=1000
library(rafalib)
mypar(3,2)
Ns<-seq(5,30,5)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B, ttest_gen(N))
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=2*N-2),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
} 


#------------------

X =sample(c(-1,1), 15, replace=TRUE)

hist(X)

tstat <- sqrt(15)*mean(X) / sd(X)



#---
set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)
#The population data is not normal thus the theory does not apply.
#We check with a Monte Carlo simulation. The qqplot shows a large tail. 
#Note that there is a small but positive chance that all the X are the same.
##In this case the denominator is 0 and the t-statistics is not defined


#-------------------
sample_gen = function(n, mean = 0, sd = 1){
  
  a = rnorm(n, mean, sd)
  return(a)  
  
}

mean_samples = replicate(1000, sample_gen(15))
hist(mean_samples)
qqnorm(mean_samples)
qqline(mean_samples)
mean(mean_samples)
sd(mean_samples)
1/sqrt(15)
