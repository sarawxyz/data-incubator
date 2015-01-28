###
###DI analysis
###


hits <- read.csv("hits.csv")

str(hits)
head(hits)

hits$time <- strptime(hits$time, "%Y-%m-%d %H:%M:%S")
hits$user <- as.factor(hits$user)
hits$category <- as.factor(hits$category)

###Question 1
###Mean # of seconds between first & last page visits, when visits >1

