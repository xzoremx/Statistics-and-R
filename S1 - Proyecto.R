
install.packages("rafalib")

library(rafalib)

install.packages("swirl")

library(swirl)

swirl()

R.version

## Prueba de Entrada:

#Pregunta 1:

vector_i = c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)

promedio = mean(vector_i)

print(promedio)

#Pregunta 2:

sumatoria = 0

for (i in 1:25){
  
  sumatoria = sumatoria + i*i
}

print(sumatoria)


#Pregunta 3:

dataset = cars

class(dataset)

View(dataset)

distanciapromedio = mean(dataset[,2])1

print(distanciapromedio)

numerofila = which(dataset[,2] == 85)

print(numerofila)



#--------------------------------------------------------------

x = 1:10
y = rnorm(10) #Funcion que:
#n es el número de valores aleatorios que se desean generar.
#mean es la media de la distribución normal.
#sd es la desviación estándar de la distribución normal.

print(y)

plot(x,y)

#-----------------------------------------------------------



#Lectura de Archivos CSV:

data1 <- read.csv("femaleMiceWeights.csv")

RNGkind()

View(data1)

head(data1)

#EJERCICIOS:

#PREGUNTA 1:

#Cómo extraer el nombre de la columna de un archivo CSV:

nombres_Columnas = names(data1)

print(nombres_Columnas)


#PREGUNTA 2:

#Cómo extraer el valor de una columna y una fila en especifico:

#Segunda columna:

Fila12Columna2 = data1[12,2]

#Estructura: [x,y]
#x representa la fila
#y representa la columna


print(Fila12Columna2)

#PREGUNTA 3

#Usa $ para separar una columna

WeightColumna = data.frame(data1$Bodyweight)
View(WeightColumna)


Valor11thFila = WeightColumna[11,]
print(Valor11thFila)

#Pregunta 4:

#Uso de la funcion Lenght()

length(data1$Diet) #La cantidad de ratones (numero de filas de uno de las colummnas)

#PREGUNTA 5:

#Obtener el vector (y su promedio) de pesos(2dcolumna) a partir del valor de dieta (primera fila)


vector_pesohf = data1$Bodyweight[data1$Diet == "hf"]
print(vector_pesohf)

promedio_vectorpesohf = mean(vector_pesohf)
promedio_vectorpesohf

#Definicion de funcion
?sample
?mean
set.seed(1)


vectorx = sample(13:24, 1)
vectorx

w = data1$Bodyweight[vectorx]
w

#------------------------------------------------------------------------------

install.packages("dplyr")


library("dplyr")
?filter

datachow = filter(data1, Diet=="chow") #Funcion de filtro, primer el dataset, luego el filtro
datachow


?select
datachow_body = select(datachow, Bodyweight) #dataset y el nombre de la columna que se quiere seleccionar
datachow_body

mean(unlist(datachow_body))
#El unlist me da solo los valores sin el cabezal



datachowdefinitivo = filter(data1, Diet == "chow") %>% select(Bodyweight) %>% unlist #De forma corrida
datachowdefinitivo

mean(datachowdefinitivo)


#----------------------EJERCICIOS------------

data2 = read.csv("msleep_ggplot2.csv")
data2


class(read.csv("msleep_ggplot2.csv"))

#PREGUNTA 2:

#Usa filter para determinar cuantos animales primates hay en la tabla

datap = filter(data2, order == "Primates")
datap

count(datap)


class(filter(data2, order == "Primates"))

class(datap)



datapp = select(datap, sleep_total)
datapp
class(datapp)


mean(unlist(datapp))

summary(data2)
summary(datapp)



#-----------------------------------------------------------


library("UsingR")

datax = father.son$fheight #dataframe
datax

dataxx = round(sample(datax, 20),1) #20 valores aleatorios y redondeados a 1 digito
dataxx


#Distribución de los datos
hist(datax, breaks = seq(floor(min(datax)), ceiling(max(datax))))



#Funcion que muestra la proporcion de personas que estan debajo de cierta estatura.
dataxs = seq(floor(min(datax)), ceiling(max(datax)), 0.1)
plot(dataxs, ecdf(datax)(dataxs), typle="l")

summary(datax)
sd(datax)
mean(datax > 70) #Este Codigo nos da la proporcion de valores de datax que son mayores a 70: 0.205

1 - pnorm(70, mean(datax), sd(datax)) #Este codigo nos da la aproximacion por una funcion normal de los datos
#Aqui la proporcion de los valores de 70 respecto al total es: 0.199


percentiles = seq(0.01, 0.99, 0.01)
quantiles = quantile(datax, percentiles)

funcion_normal = qnorm(percentiles, mean(datax), sd(datax))
plot(funcion_normal, quantiles, xlab="Normal Percentiles", ylab="Height Percentiles")
abline(0,1)


qqnorm(datax)
qqline(datax)


#------------------------------------------------

load("skew.RData")
dim(dat)
qqnorm(dat[,4])
View(dat)

for (i in 1:9) {
  qqnorm(dat[,i])
}
par(mfrow=c(1,1))

#-----------

hist(exec.pay)
qqnorm(exec.pay)
qqline(exec.pay)


boxplot(exec.pay, ylab="10,000s of dollars", ylim=c(0,400))
mean(exec.pay)
median(exec.pay) #Linea central de la caja - es el Q2 de todos los datos (ordenados de menor a mayor) 
#la parte superior de la caja es el Q3 y la inferior es el Q1 (dentro de la caja estan el 50% de todos los datos)


#----------

head(InsectSprays)

View(InsectSprays)

boxplot(split(unlist(InsectSprays$count), InsectSprays$spray))

boxplot(unlist(InsectSprays$count) ~ (InsectSprays$spray))


library(dplyr)
View(nym.2002)


data_nymMale = filter(nym.2002, gender == "Male")
View(data_nymMale)

data_nymFemale = filter(nym.2002, gender == "Female")
View(data_nymFemale)

hist(nym.2002)
hist(data_nymFemale$time)
hist(data_nymMale$time)

boxplot(unlist(data_nymMale$time) ~ (data_nymMale$age))
boxplot(unlist(data_nymFemale$time) ~ (data_nymFemale$place))

boxplot((nym.2002$time) ~ (nym.2002$gender))

median(data_nymFemale$time)
median(data_nymMale$time)

summary(data_nymMale)
summary(data_nymFemale)

qqnorm(data_nymMale$time)
qqline(data_nymMale$time)

rest_male = ks.test(data_nymMale$time, "pnorm")
rest_male


rest_female = ks.test(data_nymFemale$time, "pnorm")
rest_female

qqnorm(data_nymFemale$time)
qqline(data_nymFemale$time)

