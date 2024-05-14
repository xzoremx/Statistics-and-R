

data = read.csv("mice_pheno.csv")
data = na.omit(data)

library(dplyr)

y = filter(data, Sex == "M" & Diet == "chow") %>% select(Bodyweight) %>% unlist()
y

#Generacion de 1000 promedios de la data < y > con una muestra de 25

set.seed(1)

promedios = replicate(1000, mean(sample(y, 25)))
promedios

#Esta funcion permite mostrar dos diagramas a la vez
mypar(1,2)

#Histograma de los 1000 promedios
hist(promedios)

#Normal Q-Q plot
qqnorm(promedios)
qqline(promedios)

#1)¿Cuál es el promedio de la distribución del promedio muestral?

mean(promedios)
popsd(promedios)
