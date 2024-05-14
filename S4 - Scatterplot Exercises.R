data(nym.2002, package="UsingR")
head(nym.2002)

library(dplyr)

males = filter(nym.2002, gender == "Male")
females = filter(nym.2002, gender == "Female")

corr_pearson_male = cor(males$age, males$time)
corr_pearson_male


corr_pearson_female = cor(females$age, females$time)
corr_pearson_female


m_a = males$age
m_t = males$time

mean(males$time)

f_a = females$age
f_t = females$time

plot(m_a,m_t)
boxplot(split(m_t, round(m_a)))
print(mean(m_a[round(m_t) == 261]))

plot(f_a,f_t)
boxplot(split(f_t, round(f_a)))
