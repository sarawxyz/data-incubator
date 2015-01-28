###
###DI analysis
###

library(dplyr)

hits <- read.csv("hits.csv")

str(hits)
head(hits)

hits$time <- strptime(hits$time, "%Y-%m-%d %H:%M:%S")
hits$user <- as.factor(hits$user)
hits$category <- as.factor(hits$category)

###table of # visits per user
user_freq <-as.data.frame(table(hits$user))



###Question 1
###Mean # of seconds between first & last page visits, when visits >1

#specify user must > 1
dupes <- duplicated(hits$user)
hitsdup <- filter(hits, duplicated(user))
seconds <- difftime(max(hits$time), min(hits$time), units = "secs")


###Question 3
###Average # page visits, when visits > 1


