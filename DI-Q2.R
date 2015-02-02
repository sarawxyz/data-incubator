###data incubator Q2
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)

hits <- read.csv("hits.csv")
#hits <- tbl_df(hits)

hits$time <- strptime(hits$time, "%Y-%m-%d %H:%M:%S")
hits$user <- as.factor(hits$user)
hits$category <- as.factor(hits$category)

###table of # visits per user
user_freq <-as.data.frame(table(hits$user))
qplot(Freq, data=user_freq)   ##note: exteme skewness


###DONE BUT CHECK UNITS
###Q1: average # of seconds btwn 1st & last page visits, visits >1
###
ave_time <- with(hits, tapply(time, user, 
                             FUN = function(time) (max(time) - min(time))))
row_sub = apply(ave_time, 1, function(row) all(row !=0 ))
ave_time <- ave_time[row_sub]
average_time <- round(mean(ave_time), 10)


###
###Q2: average # seconds btwn consecutive page visits
###
ave_switch <- with(hits, tapply(time, user, 
                              FUN = function(time) mean())

###DONE
###Q3: average # page visits  
###
user_mean <- round(mean(user_freq$Freq), 10)


###DONE
###Q4: average # page visits, visits > 1  
###
multi <- subset(user_freq, Freq != 1)
multi_mean <- round(mean(multi$Freq), 10)

###DONE
###Q5: average # categories visited per user
###
num_cat <- with(hits, tapply(category, user, 
                             FUN = function(category) length(unique(category))))
ave_num_cat <- round(mean(num_cat), 10)


###DONE CHECK LOGIC
###Q6: average # categories visited per user,  visits > 1
###
user_freq <- rename(user_freq, c("Var1"="user", "Freq" = "visits"))
num_cat <- as.data.frame.table(num_cat)
num_cat <- rename(num_cat, c("Var1"="user", "Freq" = "cats"))
num_cats_multi_visits <- num_cat$user %in% multi$user
cats_multi_visits <- num_cat[num_cats_multi_visits, ]
ave_cats_multi_visits <- round(mean(cats_multi_visits$cats), 10)

###DONE
###Q7:  average # categories visited per user,  cat > 1  
###
num_cat_multi <- subset(num_cat, num_cat > 1)
ave_num_cat_multi <- round(mean(num_cat_multi), 10)


###
###Q8: Probability of immediately visiting page of same category, visits > 1
###
same_cat <- aggregate (group_by?)


###
###Q9. Highest probability of transition to different category. 
###Give a tuple 'Category1, Category2, probability'

