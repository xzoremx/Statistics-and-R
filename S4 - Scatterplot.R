library(rafalib)
data(father.son,package="UsingR") ##available from CRAN
head(father.son)

f = father.son$fheight
s = father.son$sheight

plot(f,s)
boxplot(split(s, round(f)))
print(mean(s[round(f) == 72]))



f = (f - mean(f))/sd(f)
s = (s - mean(s))/sd(s)

promedios = tapply(s,round(f*4)/4,mean)
promedios

fatherheights = as.numeric(names(promedios))
plot(fatherheights, promedios)
abline(0,cor(f,s))
