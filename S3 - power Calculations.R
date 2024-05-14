library(dplyr)

dat = read.csv("mice_pheno.csv")

controlpop = filter(dat, Sex == "F" & Diet =="chow") %>% select(Bodyweight) %>% unlist()
hfpop = filter(dat, Sex == "F" & Diet =="hf") %>% select(Bodyweight) %>% unlist()

mu_hf = mean(hfpop)
mu_control = mean(controlpop)

print(mu_hf - mu_control)

set.seed(1)
N = 5
hf = sample(hfpop, N)

control = sample(controlpop, N)

t.test(hf,control)$p.value






B = 2000

reject = function(N, alpha = 0.05){
  hf = sample(hfpop, N)
  control = sample(controlpop, N) 
  pval = t.test(hf,control)$p.value
  pval = alpha
}

rejections = replicate(B, reject(N))
mean(rejections)

#-------------------
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table(filename, header=TRUE)

bwt.nonsmoke <- filter(babies, smoke == 0) %>% select(bwt) %>% unlist()
bwt.smoke <- filter(babies, smoke == 1) %>% select(bwt) %>% unlist()

set.seed(1)
B <- 10000
alphalevel <- 0.01
powers <- numeric()

sizes <- c(30, 60, 90, 120)

for (N in sizes) {
  reject <- function(N, alpha = 0.01){
    bwtnons <- sample(bwt.nonsmoke, N)
    bwts <- sample(bwt.smoke, N) 
    pval <- t.test(bwtnons, bwts)$p.value
    if(pval < alpha) {
      return(1) # Rechaza la hipótesis nula
    } else {
      return(0) # No rechaza la hipótesis nula
    }
  }
  
  rejections <- replicate(B, reject(N))
  power <- mean(rejections)
  powers <- c(powers, power)
}

powers


