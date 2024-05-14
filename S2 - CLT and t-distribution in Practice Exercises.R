library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)



set.seed(1)  # Set the seed for reproducibility

# Define parameters
n = 100  # Number of dice rolls
repetitions = 30  # Number of repetitions

# Perform the simulation
rolls <- replicate(repetitions, {
  x <- sample(1:6, n, replace=TRUE)
  mean(x == 6)
})

# Calculate z-scores
p = 0.5   # Probability of rolling a 6
z_scores <- (rolls - p) / sqrt(p * (1 - p) / n)

# Calculate proportion of times z was larger than 2 in absolute value
prop_large_z <- mean(abs(z_scores) > 2)
prop_large_z

hist(z_scores)
qqnorm(z_scores)
qqline(z_scores)



#----------
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
Xprom_muestral = mean(X)
Yprom_muestral = mean(Y)
# 4. Xprom follows a normal distribution with mean ux  and 
#standard deviation  ox/sqrt(12) where ox is the population standard deviation.


ox = sd(X) #Desviacion estandar de la poblacion(es una estimacion)

# Definir los parámetros
n = 12  # Tamaño de la muestra
  error = 2  # Error máximo permitido en gramos

# Calcular la probabilidad de que Xprom este lejos en 2 gramos de ux - utilizando el TLC
prob <- 2 * pnorm(-error / (ox / sqrt(n)))

# Imprimir la probabilidad
print(prob)



#-----

oy = sd(Y)


#Error estandar

SE= sqrt(((ox*ox)/12) + ((oy*oy)/12))
SE

#
t.test(Y,X)
#



#T estadistico:
obs = abs(mean(Y) - mean(X))

t_est = obs/SE
t_est

#P - Value
2* (1 - pnorm(t_est))


#-----

qqnorm(X)
qqnorm(Y)


#En resumen, la diferencia en los valores p obtenidos puede deberse a que la distribución t-distribuida (t.test)
#tiene en cuenta la variabilidad introducida por la estimación del error estándar(asume que no se conoce), 
#lo que hace que los valores extremadamente grandes sean más probables bajo la hipótesis nula(punto de vista pvalue), 
#lo que puede llevar a valores p más grandes en comparación con la distribución normal (t.test mayor que pnorm).
1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)
