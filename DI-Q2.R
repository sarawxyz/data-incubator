###data incubator Q2
library(plyr)
library(dplyr)
library(ggplot2)

hits <- read.csv("hits.csv")
#hits <- tbl_df(hits)

hits$time <- strptime(hits$time, "%Y-%m-%d %H:%M:%S")
hits$user <- as.factor(hits$user)
hits$category <- as.factor(hits$category)

###table of # visits per user
user_freq <-as.data.frame(table(hits$user))
qplot(Freq, data=user_freq)   ##note: exteme skewness


###Q1: average # of seconds btwn 1st & last page visits, visits >1
#users <- group_by(hits, user)
first_last_diff <- difftime(max(users$time), min(users$time), units = "secs")
ave <- tapply(hits$time, hits$user, (difftime(max(user$time), min(user$time), units = "secs")))
secs_start <- ddply(hits, .(user), mutate, secs_from_start = time - min(time))


#specify user must > 1
#dupes <- duplicated(hits$user)
#hitsdup <- filter(hits, duplicated(user))
#seconds <- difftime(max(hits$time), min(hits$time), units = "secs")


###Q2: average # seconds btwn consecutive page visits
hits_o <- hits[order(hits$time),]
ave_switch <- average(difftime)


###Q3: average # page visits  DONE
user_mean <- round(mean(user_freq$Freq), 10)


###Q4: average # page visits, visits > 1  DONE
multi <- subset(user_freq, Freq != 1)
multi_mean <- round(mean(multi$Freq), 10)

###Q5: average # categories visited per user  DONE
num_cat <- with(hits, tapply(category, user, 
                             FUN = function(category) length(unique(category))))
ave_num_cat <- round(mean(num_cat), 10)




###Q6: average # categories visited per user,  visits > 1    DONE CHECK LOGIC
user_freq <- rename(user_freq, c("Var1"="user", "Freq" = "visits"))

num_cat <- as.data.frame.table(num_cat)
num_cat <- rename(num_cat, c("Var1"="user", "Freq" = "cats"))

#num_cat_multi <- as.data.frame.table(num_cat_multi)
#num_cat_multi <- rename(num_cat_multi, c("Var1"="user", "Freq" = "cats"))

num_cats_multi_visits <- num_cat$user %in% multi$user
cats_multi_visits <- num_cat[num_cats_multi_visits, ]
ave_cats_multi_visits <- round(mean(cats_multi_visits$cats), 10)


###Q7:  average # categories visited per user,  cat > 1   DONE
num_cat_multi <- subset(num_cat, num_cat > 1)
ave_num_cat_multi <- round(mean(num_cat_multi), 10)


###Q8: Probability of immediately visiting page of same category, visits > 1
same_cat <- aggregate (group_by?)

###Q9. Highest probability of transition to different category. 
###Give a tuple 'Category1, Category2, probability'

