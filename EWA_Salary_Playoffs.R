library(dplyr)
library(ggplot2)

#Read in the data files
sal2016 <- read.csv("C:/Users/Peter/Desktop/Utica/Capstone-680/InputData/2016-17NBASalaries.csv")
EWA2016 <- read.csv("C:/Users/Peter/Desktop/Utica/Capstone-680/InputData/2016-17EWA.csv")

#Remove non-numerical characters from the Base column
sal2016$Base <- as.numeric(gsub('[$,]', '', sal2016$Base))

#Merge the datasets together (Both use a Player column)
mrg <- merge(EWA2016, sal2016)

subset(mrg,(MPG < 12) & (Base < 3000000) & EWA < 2 & EWA > -2)

boxplot(mrg$EWA ~ mrg$Team)
boxplot(mrg$Base ~ mrg$Team)

#The boxplots revealed an issue with the methodology. Some players switched
#teams during the season. The data for the EWA data set showed this by putting
#all the teams in the Team column separated by a slash. However, the EWA stat
#is calculated using all of the games played. Therefore, attributing that EWA
#for such a player is problematic. Omitting the player is also problematic. Is
#it acceptable to prorate the EWA stat based on the percentage of the games
#played per team? Should it just go to the team which played the most games?
#Issue resolved as all the players except Kyle Korver played less more than 30
#games with only one team. The previous stats2016 data set filters this, so it
#will be easiest to merge EWA with it based on the Player category.