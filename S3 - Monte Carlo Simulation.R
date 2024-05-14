set.seed(1)

library(rafalib)

dat = read.csv("mice_pheno.csv")

controlPopulation <- read.csv("femaleMiceWeights.csv")$Bodyweight
head(controlPopulation)
controlPopulation = unlist((controlPopulation))
controlPopulation

#Funcion de t-test:

ttestgenerator = function(n){
  
  cases = sample(controlPopulation, n)
  control = sample(controlPopulation, n)
  
  tstat <- (mean(control) - mean(cases)) / sqrt((var(control) + var(cases)) / n)
  return(tstat)
}


ttest_resultados = replicate(1000, ttestgenerator(3))

hist(ttest_resultados)
qqnorm(ttest_resultados)
qqline(ttest_resultados)




# t-Distribution

ps = (seq(0.999)+0.5)/1000
qqplot(qt(ps, df = 2*3-2), ttest_resultados, xlim = c(-6,6), ylim = c(-6,6))
abline(0,1)



#----------------------------


controls = rnorm(5000, mean = 24, sd = 3.5)

ttestgen2 = function(n, mean = 24, sd = 3.5){
  
  cases = rnorm(n, mean, sd)
  controls = rnorm(n, mean, sd)
  tstat <- (mean(controls) - mean(cases)) / sqrt((var(controls) + var(cases)) / n)
  return(tstat)  

}

ttest_resultados2 = replicate(1000, ttestgen2(3))
qqnorm(ttest_resultados2)
qqline(ttest_resultados2)
