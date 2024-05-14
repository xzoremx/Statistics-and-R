data = read.csv("FemaleMiceWeights.csv")

library(dplyr)

control = filter(data, Diet == "chow") %>% select(Bodyweight) %>% unlist()
treatment = filter(data, Diet == "hf") %>% select(Bodyweight) %>% unlist()

t.test(treatment, control)

qqnorm(treatment)
qqline(treatment)

qqnorm(control)
qqline(control)
