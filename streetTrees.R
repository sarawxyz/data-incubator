###vancouver street trees
library(dplyr)
library(ggplot2)

setwd("/Users/saraweinstein/Documents/Data Science/street_trees/datafiles/")

temp = list.files(pattern="*.csv")  #list all .csv
myFiles = lapply(temp, read.csv)    #read them all in
str(myFiles)

trees <- do.call("rbind", myFiles)  #rbind into one large data frame
str(trees)

rm(myFiles)
rm(temp)
setwd("/Users/saraweinstein/Documents/Data Science/street_trees/")

###remove unwanted columns
#drop <- c("PLANT_AREA", "ROOT_BARRIER", "CURB", )   #code to drop by name
#trees <- trees[,!(names(trees) %in% drop)]
#trees <- trees[, -c(5:9, 13:15) ]   #remove specified columns

###convert dates & calculate age of trees
library("lubridate")
trees$DATE_PLANTED <- ymd(trees$DATE_PLANTED)
today <- Sys.Date()
trees$AGE <- difftime(today, trees$DATE_PLANTED, units="days")  
trees$AGE <- as.numeric(trees$AGE) 

#wrap trees in tbl_df to prevent printing
#trees <- tbl_df(trees)  

#calculate age in years
trees <- mutate(trees, AGE_YEARS = AGE/365)         

##create data.frame w average tree diameter & # of trees per neighbourhood
nbhd_trees <- trees %>% 
      group_by(NEIGHBOURHOOD_NAME) %>%            
      summarize(                                  
          diam = mean(DIAMETER),                  
          ntrees = length(TREE_ID),
          height = mean(HEIGHT_RANGE_ID)
          )

#read in areas calculated from gis data (.kml file)
CLA <- read.csv("CLA_areas.csv")         

#make sure both data.frames are in same order, add area as new column to nbhd_trees
nbhd_trees <- nbhd_trees[order(nbhd_trees$NEIGHBOURHOOD_NAME), ]
CLA <- CLA[order(CLA$NAME), ]         
nbhd_trees$NBHD_AREA <- CLA$AREA..sq..km. 

#re-sequence the row.names
row.names(nbhd_trees) <- NULL         
nbhd_trees$NEIGHBOURHOOD_NAME <- droplevels(nbhd_trees$NEIGHBOURHOOD_NAME)

#calculate tree density per 100m2, round all cols
nbhd_trees$density <- (nbhd_trees$ntrees / nbhd_trees$NBHD_AREA) / 10  
nbhd_trees$density <- round(nbhd_trees$density, digits = 2)
nbhd_trees$diam <- round(nbhd_trees$diam, digits = 2)
nbhd_trees$NBHD_AREA <- round(nbhd_trees$NBHD_AREA, digits = 2)

#create 'forest mass' index
nbhd_trees$forest_mass <- nbhd_trees$diam * nbhd_trees$height * nbhd_trees$density
glimpse(nbhd_trees)
nbhd_trees <- nbhd_trees[ , 6:7]

#pull in census data, select cols, merge w trees, write out file for
#mapping
census <- read.csv("census_data_2006.csv")
glimpse(census)
census_vals <- census %>%
      select(Neighbourhood:UnemploymentRate, MedianHouseholdIncome, 
             X.LowIncomeBeforeTax, X25to64Educ)
census_vals <- census_vals[1:22, ]
census_vals <- droplevels(census_vals)
nbhd_trees <- cbind(census_vals, nbhd_trees)

#convert median income from dollar amount factor to numeric, convert pop to numeric 
nbhd_trees$MedianHouseholdIncome <- as.character(nbhd_trees$MedianHouseholdIncome)
nbhd_trees$MedianHouseholdIncome <- gsub("(\\$|,)","", nbhd_trees$MedianHouseholdIncome)
nbhd_trees$MedianHouseholdIncome <- as.numeric(nbhd_trees$MedianHouseholdIncome)

nbhd_trees$Total.Pop <- as.character(nbhd_trees$Total.Pop)
nbhd_trees$Total.Pop <- gsub("(\\,)","", nbhd_trees$Total.Pop)
nbhd_trees$Total.Pop <- as.numeric(nbhd_trees$Total.Pop)

#write out file for mapping
write.csv(nbhd_trees, file = "NBHD_Trees.csv", row.names = FALSE)

rm(trees)
rm(CLA)
rm(census)
rm(census_vals)

#correlations
nbhd_list <- nbhd_trees$Neighbourhood
nbhd_trees <- nbhd_trees[ ,2:13]
correl <- cor(nbhd_trees)
correl <- correl[, 12]

#rm outlier = shaunnessy, check correlations
nbhd_trees2 <- nbhd_trees[-18, ]
correl2 <- cor(nbhd_trees2)
correl2 <- correl2[, 12]

#graphs
tree_corr <- nbhd_trees[ , c(7:10, 12)]
pairs(tree_corr, colour="gray20") +
      geom_smooth(method="lm")

income <- ggplot(nbhd_trees, aes(x=forest_mass, y=MedianHouseholdIncome)) +
      geom_point(size=4, colour = "orange") +  
      geom_smooth(method=lm, se = FALSE, colour = "black") +
      xlab("\nForest Mass") +
      ylab("Median Household Income\n") +
      coord_cartesian(ylim = c(0, 125000)) +
      ggtitle("Correlation between Income and Forest Mass by Neighbourhood\n\n") +
      annotate("text", x = 2000, y = 90000, label = "r^2 = 0.53") +
      theme_minimal()

uirate <- ggplot(nbhd_trees, aes(x=forest_mass, y=UnemploymentRate)) +
      geom_point(size=4, colour = "#56B4E9") +  
      geom_smooth(method=lm, se = FALSE, colour = "black") +
      xlab("\nForest Mass") +
      ylab("Unemployment Rate\n") +
      coord_cartesian(ylim = c(0, 12.5)) +
      ggtitle("Correlation between Unemployment Rate and Forest Mass by Neighbourhood\n\n") +
      annotate("text", x = 6000, y = 9, label = "r^2 = -0.49") +
      theme_minimal()

png(filename = "Income.png")
income
dev.off()

png(filename = "UIRate.png")
uirate
dev.off()