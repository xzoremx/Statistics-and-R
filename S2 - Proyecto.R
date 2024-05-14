
library(dplyr)

data1 = read.csv("femaleMiceWeights.csv")

View(data1)

data1_chow = filter(data1, Diet == "chow")

data1_chow = select(data1_chow, Bodyweight)

data1_chow = unlist(data1_chow)
data1_chow
mean(data1_chow)


data1_highfat = filter(data1, Diet == "hf") %>% select(Bodyweight) %>% unlist
data1_hightfat
mean(data1_highfat)


diference_mean = mean(data1_highfat) - mean(data1_chow)
diference_mean


poblacion = read.csv("femaleControlsPopulation.csv")
poblacion = unlist(poblacion)

obs_null_dif = abs(mean(data1_chow) - mean(data1_highfat))

#-

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
mean(poblacion)
set.seed(1)


diferencia_promedio1 = abs(mean(sample(poblacion, 5)) - mean(poblacion))
diferencia_promedio1

set.seed(5)

diferencia_promedio2 = abs(mean(sample(poblacion, 5)) - mean(poblacion))
diferencia_promedio2

#-


n = 10000
vector_null_dif = vector("numeric", n)

for(i in 1:n){
  chow = (sample(poblacion, 12))
  highfat = (sample(poblacion, 12))
  vector_null_dif[i] = mean(chow) - mean(highfat)

}
max(vector_null_dif)
hist(vector_null_dif)


mean(abs(vector_null_dif) > obs_null_dif) #P-Value


obs_null_dif

set.seed(1)
m = 10000
vector_null_avg = vector("numeric", m)
x <- unlist(poblacion)
mean(x)


for(i in 1:m){
  
  randomsample = sample(poblacion, 5)
  vector_null_avg[i] = mean(randomsample)
}

mean(vector_null_avg > mean(x)) #Proporcion de los promedios (vector null) que son mayores al promedio de x

proporcion <- mean(abs(vector_null_avg - mean(x)) > 1)
proporcion #Proporcion de los promedios que son mayores al promedio de x en 1 gramo

#------------------------------------

install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)
View(gapminder)

vector_x = filter(gapminder, year == "1952") %>% select(lifeExp) %>% unlist()
hist(vector_x)
vector_x

mean(vector_x <= 40) #Proporcion de paises que tienen una lifeExp menor o igual a 40

prop = function(q) {
  mean(vector_x <= q)
}
prop(40) #Proporcion de paises que tiene una lifeExp menor o igual a 40


qs = seq(from=min(vector_x), to=max(vector_x), length=20)
print(qs)
props = sapply(qs, prop)
plot(qs, props)



plot(ecdf(vector_x))


#----

# hace promedio de una muestra de 5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X1 <- sample(poblacion,5)
  averages5[i] <- mean(X1)
}
X1
# hace promedio de una muestra de 50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X2 <- sample(poblacion,50)
  averages50[i] <- mean(X2)
}
X2


hist(averages5)
hist(averages50)

a = mean(averages50 >= 23 & averages50 <= 25)
a


z = pnorm(25, 23.9, 0.43) - pnorm(23, 23.9, 0.43)
z

?pnorm

#----------------------------------------

l

