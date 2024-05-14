data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
head(chick)
chick = na.omit(chick)
View(chick)

library(dplyr)

x = filter(chick, Diet == "1") %>% select(weight.4) %>% unlist()
y = filter(chick, Diet == "4") %>% select(weight.4) %>% unlist()

t.test(x,y)
wilcox.test(x,y)


new_x = c(x,200)

t.test(new_x,y)

wilcox.test(new_x,y)



#---

library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)


t.test(x,y+10)$statistic - t.test(x,y+100)$statistic
