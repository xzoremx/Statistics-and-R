set.seed(1)

chowPopulation = read.csv("femaleControlsPopulation.csv")
chowPopulation = unlist(chowPopulation)

mu_chow = mean(chowPopulation)
print(mu_chow)

N = 30
chow = sample(chowPopulation, N)
print(mean(chow))


se = sd(chow)/sqrt(N)
se


Q = qnorm(1 - 0.05/2)


-Q < (mean(chow)-mean(chowPopulation)) / se < Q


intervalo = c(mean(chow) - Q*se, mean(chow)+Q*se)
intervalo

intervalo[1] < mu_chow & intervalo[2] > mu_chow



library(rafalib)
B= 250
mypar()
plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n",xlab="Weight",ylab="intervalo",ylim=c(1,B))
abline(v=mean(chowPopulation))

for(i in 1:B){
  chow = sample(chowPopulation, N)
  se = sd(chow)/sqrt(N)
  intervalo = c(mean(chow) - Q*se, mean(chow)+Q*se)
  covered <- mean(chowPopulation) <= intervalo[2]
  color <- ifelse(covered,1,2)
  lines(intervalo, c(i,i), col = color)
}
