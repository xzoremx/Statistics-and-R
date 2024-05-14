data(ChickWeight)
head(ChickWeight)
?ChickWeight
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
head(chick)
chick = na.omit(chick)
View(chick)


day_4_weights = c(chick$weight.4, 3000)
day_21_weights = c(chick$weight.21, 3000) 

(mad(day_4_weights))/mad(chick$weight.4)


pearson1 = cor(chick$weight.4,chick$weight.21)
pearson2 = cor(day_4_weights, day_21_weights)


pearson2/pearson1
