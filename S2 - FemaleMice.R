data = read.csv("FemaleMiceWeights.csv")

library(dplyr)

control = filter(data, Diet == "chow") %>% select(Bodyweight) %>% unlist()
treatment = filter(data, Diet == "hf") %>% select(Bodyweight) %>% unlist()

N1 = length(treatment)


obs = abs(mean(control) - mean(treatment))

#Error estandar

ES = sqrt(var(treatment)/N + var(control)/N)

#T estadistico:

t_est = obs/ES

#P-Value

2* (1 - pnorm(t_est))


#Teoria:

####Hipótesis Nula y Alternativa####
#probar si hay alguna diferencia significativa entre dos grupos o condiciones


####Estadístico T####
#Es una medida que se utiliza para cuantificar la diferencia entre los dos grupos. 
#En este caso, el estadístico t se calcula como la diferencia entre los promedios de
#los dos grupos dividida por el error estándar de esa diferencia. 
#Cuanto mayor sea el valor absoluto del estadístico t, 
#mayor será la diferencia observada entre los dos grupos.



##P-Value###
#te dice qué tan probable es que la diferencia que observaste
#en los datos reales ocurra si asumes que no hay ningun tipo de relacion en las variables.
#"¿Qué tan probable es que obtengamos estos resultados (diferencia de pesos), 
#o resultados más extremos, si asumimos que 
#no hay realmente ninguna diferencia entre los grupos en la población? (hipotesis nula true)"





#---
population = read.csv("femaleControlsPopulation.csv")
population = na.omit(population)
population = unlist(population)


library(dplyr)


n = 10000
vector_null_dif = vector("numeric", n)

for(i in 1:n){
  chow = (sample(population, 3))
  highfat = (sample(population, 3))
  SE = sqrt(var(treatment)/3 + var(control)/3)
  vector_null_dif[i] = (mean(chow) - mean(highfat))/SE
  
  
}
vector_null_dif

library(rafalib)
mypar()
max(vector_null_dif)
hist(vector_null_dif)


qqnorm(vector_null_dif)
qqline(vector_null_dif)
abline(0,1)
