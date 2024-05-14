
d = read.csv("assoctest.csv")
head(d)

d = table(d)

chisq.test(d)

fisher.test(d)


