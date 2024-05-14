url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)


bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)


set.seed(1)

dat.ns = sample(bwt.nonsmoke, 25)
dat.s = sample(bwt.smoke, 25)

# Paso 1: Calcular la diferencia entre las medias

diff_means <- mean(dat.ns) - mean(dat.s)

# Paso 2: Calcular el error estándar de la diferencia

se_diff <- sqrt(var(dat.ns)/25 + var(dat.s)/25)

# Paso 3: Encontrar los valores críticos t


df <- 2*25 - 2 # grados de libertad


t_crit <- qt(0.995, df) # 99% CI

# Paso 4: Calcular el margen de error (ESTO ES LO NUEVO)
margin_of_error <- t_crit * se_diff
margin_of_error


# Paso 5: Calcular el intervalo de confianza
lower_bound <- diff_means - margin_of_error
upper_bound <- diff_means + margin_of_error

#----

set.seed(1)

# Tomar una muestra aleatoria de tamaño 5 de cada conjunto de datos
sample_size <- 5
sample_ns <- sample(bwt.nonsmoke, sample_size)
sample_s <- sample(bwt.smoke, sample_size)

# Realizar una prueba t de dos muestras
t_test_result <- t.test(sample_ns, sample_s)

# Obtener el valor p
p_value <- t_test_result$p.value

# Mostrar el valor p
print(p_value)

