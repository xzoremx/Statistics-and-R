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

# Step 1: Determinamos la media de ambas muestras
mean_ns <- mean(dat.ns)
mean_s <- mean(dat.s)

# Step 2: Determinamos la desviacion estandar de ambas muestras
sd_ns <- sd(dat.ns)
sd_s <- sd(dat.s)

# Step 3: Determinamos el error estandar de ambas muestras
n_ns <- length(dat.ns)
n_s <- length(dat.s)
se <- sqrt((sd_ns^2 / n_ns) + (sd_s^2 / n_s))

# Step 4: hallamos el  t-statistic
tval <- (mean_ns - mean_s) / se

# Abs de t-statistic
abs_tval <- abs(tval)
abs_tval

pval <- 1 - (pnorm(abs(tval)) - pnorm(-abs(tval)))

pval


