
data(nym.2002, package="UsingR")
head(nym.2002)
time = sort(nym.2002$time)
median_time = median(time)
median_time
min(time)

division1 = min(time)/median(time)
division1


division2 = max(time)/median(time)
division2
