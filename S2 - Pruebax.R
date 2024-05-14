library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat = read.csv(filename)


dat = na.omit(dat) 
View(dat)


library(dplyr)

datx = filter(dat, Diet == "chow" & Sex == "F") %>% select(Bodyweight) %>% unlist()
datx

xxx = mean(datx) #Media de la data de Chow y Male (Bodyweight)
xxx

library(rafalib)


popsd(datx) #Desviacion estandar

set.seed(2)
rdsx = sample(datx, 25)
aaa = mean(rdsx)
aaa

daty = filter(dat, Diet == "hf" & Sex == "F") %>% select(Bodyweight) %>% unlist()
yyy = mean(daty)
yyy

popsd(daty)

set.seed(2)
rdsy = sample(daty, 25)
bbb =mean(rdsy)
bbb

dif_mean_sample = bbb - aaa
dif_mean_sample
dif_mean_datxy = yyy - xxx
dif_mean_datxy
dif_abs = abs(dif_mean_datxy - dif_mean_sample)
dif_abs


#--------------------------------------------

?pnorm

# Suppose your list of numbers is stored in a vector called 'data'
data =  #ESTO ES UNA DATA NORMAL
# Calculate the mean and standard deviation of the data
mean_data <- mean(data)
sd_data <- sd(data)

# Calculate the lower and upper bounds for one standard deviation away from the mean
lower_bound <- mean_data - 1*sd_data
upper_bound <- mean_data + 1*sd_data

# Calculate the proportion of numbers within one standard deviation away from the mean
proportion <- pnorm(upper_bound, mean_data, sd_data) - pnorm(lower_bound, mean_data, sd_data)

# Print the proportion
print(proportion)
###############################################################################################


# Instala y carga el paquete rafalib
install.packages("rafalib")
library(rafalib)

library(dplyr)

# Define y como los pesos de los machos en la dieta de control
y <- filter(dat, Sex == "M" & Diet == "chow") %>% select(Bodyweight) %>% unlist()
y
# Calcula la media y la desviación estándar poblacional de los datos
mean_y <- mean(y)
mean_y
sd_y <- popsd(y)
sd_y
sds = sd(y)
sds
?popsd

# Calcula los límites inferior y superior para una desviación estándar de la media
lower_bound <- mean_y - sd_y
upper_bound <- mean_y + sd_y

# Calcula la proporción de ratones dentro de una desviación estándar de la media
proportion <- mean(y >= lower_bound & y <= upper_bound)

# Imprime la proporción
print(proportion)
