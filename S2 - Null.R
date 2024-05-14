

population = read.csv("femaleControlsPopulation.csv")
population = na.omit(population)
population = unlist(population)


library(dplyr)


n = 10000
vector_null_dif = vector("numeric", n)

for(i in 1:n){
  chow = (sample(population, 12))
  highfat = (sample(population, 12))
  vector_null_dif[i] = mean(chow) - mean(highfat)
  
}
vector_null_dif
max(vector_null_dif)
hist(vector_null_dif)


qqnorm(vector_null_dif)
qqline(vector_null_dif)
