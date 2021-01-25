library(dplyr)
library(ggplot2)
library(cluster)
library(kernlab)
library(e1071)
library(caret)

#Turn off scientific notation
options(scipen=999)

#Filters data for the stats<year> data set
filterStatData <- function (stats) {
  #Keep only players who played at least 500 minutes during the season
  #(6 minutes per game)
  stats <- subset(stats, MP > 500)
  #Keep only those who have played more than 30 games
  stats <- filter(stats, Games > 30)
  #Remove the rows with TOT for Tm since this represents the Total for players
  #with more than one team.
  stats <- subset(stats, Tm != 'TOT')
}

#Creates a datastructure to be used for SVM classification. A dataframe for
#team data is passed in as the x argument and the dataframe to be modified is
#passed in as the y parameter
createDataForSVM <- function(x,y) {
  #TOT = 29 #Index of the team name 'TOT'
  
  #Create the variables for needed for SVM
  WCTot <- as.vector(tapply(x$wincost, x$Tm, sum)) #The aggregate of wincost per team
  EWATot <- as.vector(tapply(x$EWA, x$Tm, sum))    #The aggregate of EWA per team
  SalTot <- as.vector(tapply(x$Salary, x$Tm, sum)) #The aggregate of Salary per team
  
  #Remove pesky 'TOT' Team value
  #WCTot <- WCTot[-TOT]
  #EWATot <- EWATot[-TOT]
  #SalTot <- SalTot[-TOT]
  
  #Add the new columns to the dataframe
  y$WCTot <- WCTot
  y$EWATot <- EWATot
  y$SalTot <- SalTot
  y
}

#Location of the directory containing all the input files
inDir <- "C:/Users/Peter/Desktop/Utica/Capstone-680/InputData/"

#Read in the 2016-2017 data files
sal2017 <- read.csv(paste(inDir, "2016-17NBASalaries.csv", sep = ""))
stats2017 <- read.csv(paste(inDir, "2016-17PlayerStatsPer100.csv", sep = ""))
advStats2017 <- read.csv(paste(inDir, "2016-17AdvancedStats.csv", sep = ""))
EWA2017 <- read.csv(paste(inDir, "2016-17EWA.csv", sep = ""))
Playoffs2017 <- read.csv(paste(inDir, "2016-17Playoffs.csv", sep = ""))

#Read in the 2017-2018 data files
sal2018 <- read.csv(paste(inDir, "2017-18NBASalaries.csv", sep = ""))
stats2018 <- read.csv(paste(inDir, "2017-18PlayerStatsPer100.csv", sep = ""))
#advStats2018 <- read.csv(paste(inDir, "2017-18AdvancedStats.csv", sep = ""))
EWA2018 <- read.csv(paste(inDir, "2017-18EWA.csv", sep = ""))
Playoffs2018 <- read.csv(paste(inDir, "2017-18Playoffs.csv", sep = ""))

#Filter the stats<year> datasets
stats2017 <- filterStatData(stats2017)
advStats2017 <- filterStatData(advStats2017)
stats2018 <- filterStatData(stats2018)

#Make salary just numbers
sal2017$Salary <- as.numeric(gsub('[$,]', '', sal2017$Salary))
sal2018$Salary <- as.numeric(gsub('[$,]', '', sal2018$Salary))

#Make the Playoffs column into a factor
Playoffs2017$Playoffs <- as.factor(Playoffs2017$Playoffs)
Playoffs2018$Playoffs <- as.factor(Playoffs2018$Playoffs)

cd <- advStats2017[,c(9,10,11,12,13,14,15,16,17,18,19,20,21)]

#Some EDA for salary and EWA
boxplot(sal2017$Salary / 1000000, main = "Salary for 2017 in Millions US$", ylab="Salary in Millions US$")
boxplot(sal2018$Salary / 1000000, main = "Salary for 2018 in Millions US$", ylab="Salary in Millions US$")
boxplot(EWA2017$EWA, main = "EWA for 2017", ylab="Estimated Wins Added (EWA)")
boxplot(EWA2018$EWA, main = "EWA for 2018", ylab="Estimated Wins Added (EWA)")
hist(sal2017$Salary / 1000000, main = "Salary for 2017 in US$ Millions", xlab = "Salary in Millions US Dollars")
hist(sal2018$Salary / 1000000, main = "Salary for 2018 in US$ Millions", xlab = "Salary in Millions US Dollars")
hist(EWA2017$EWA, main = "Estimated Wins Added (EWA) for 2017", xlab = "Estimates Wins Added (EWA)")
hist(EWA2018$EWA, main = "Estimated Wins Added (EWA) for 2018", xlab = "Estimates Wins Added (EWA)")

#Different groups of stats for k-means
clusdata <- stats2017[,c(12,13,15,17,21,22,24,25,26,27,28,29)]
clusdata2 <- stats2017[,c(12,17,19,20,21,22,24,25,26,27,29)]
clusdata3 <- stats2017[,c(11,12,18,23,24,25,26,27,29)]
clusdata4 <- stats2017[,c(10,11,18,20,21,24,26,29)]
cd <- advStats2017[,c(3,9,10,11,12,13,15,16,17,18,19,20,21)]

set.seed(8675309) #Set seed for reproducibility
krange <- 5:15 #K range from 5 to 15
trials <- 100  #Number of times to run k means
totw.ss <- integer(length(krange)) #Empty vector to hold points
for (k in krange) {
  tmp.totw.ss <- integer(trials) #Empty vector to hold the trials
  for (x in 1:trials) {
    #tmp <- kmeans(stats2017[,-c(1,2,3,4,5,6,7,8,10,14,16,19,23,31,32)],centers = k, nstart = 25)
    #tmp <- kmeans(stats2017[,c(12,13,15,17,21,22,24,25,26,27,28,29)], centers = k, nstart = 25)
    tmp <- kmeans(stats2017[,c(4,31,32)], centers = k, nstart = 25)
    #tmp <- kmeans(stats2017[,c(4,10,11,12,13,14,16,17,18,19,20,21,22,23,25,26,27,28,29,30)], centers = k, nstart = 20)
    #tmp <- kmeans(cd, centers = 8, nstart = 25)
    tmp.totw.ss[x] <- tmp$tot.withinss
  }
  totw.ss[k-4] <- mean(tmp.totw.ss) #average of the withinss
}

#create scree plot to identify elbow for best value of k
plot(krange, totw.ss, type = "b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")

#tmpCls1 <- kmeans(stats2017[,c(4,10,11,12,13,14,16,17,18,19,20,21,22,23,25,26,27,28,29,30)], centers = 7, nstart = 20)
tmpCls1 <- kmeans(stats2017[,c(4,12,13,17,21,22,24,25,26,27,28,29)], centers = 7, nstart = 20)
#Create a new column for the cluster
stats2017$newPos <- tmpCls1$cluster
t <- stats2017[,c(4,12,13,17,21,22,24,25,26,27,28,29)]

#t <- stats2017[,c(4,10,11,12,13,14,16,17,18,19,20,21,22,23,25,26,27,28,29,30)]
tmpClsPlot1 <- clusplot(t, tmpCls1$cluster, color = TRUE, main = "K-Means k = 7 with traditional statistics")
#Table of traditional position vs cluster
table(stats2017$Pos, stats2017$newPos)

tmpCls <- kmeans(stats2017[,c(4,31,32)], centers = 8, nstart = 25)
#Add new position cluster to the dataset
stats2017$newPos <- tmpCls$cluster
t <- stats2017[,c(31,32)]
tmpClsPlot <- clusplot(t, tmpCls$cluster, color = TRUE, main = "K-Means with Position, Ortg, Drtg")
#Dendogram
den <- hclust(dist(t))
plot(den)

#Look at the results
table(stats2017$Pos, stats2017$newPos)

##Merge the data sets together for further EDA and analysis
#Merge the EWA and salary data for 2017
mrg17 <- merge(EWA2017, sal2017)
#Merge in the stats data as well for 2017
mrg17 <- merge(mrg17, stats2017, by.x ="Player", by.y="Player") #TODO ?? is this still neeeded?

#Merge the EWA and salary data for 2018
mrg18 <- merge(EWA2018, sal2018)
mrg18 <- merge(mrg18, stats2018, by.x ="Player", by.y="Player") #TODO ?? is this still neeeded?

#Variable for ordering the medians for the boxplot
bymedian <- with(mrg17, reorder(Tm, EWA, median))
boxplot(mrg17$EWA ~ bymedian, main = 'EWA by Team 2017', ylab = 'Estimated Wins Added (EWA)', las = 3)
#boxplot(mrg17$EWA ~ mrg17$Tm, at=rank(tapply(mrg17$EWA , mrg17$Tm, median)))
byMedSal2017 <- with(mrg17, reorder(Tm, Salary, median))
boxplot(mrg17$Salary / 1000000 ~ byMedSal2017, main = 'Salary by Team 2017 in Millions US$', ylab = 'Salary in Millions US$', las = 3)


#Variable for ordering the medians for the boxplot
byMedEWA2018 <- with(mrg18, reorder(Tm, EWA, median))
boxplot(mrg18$EWA ~ byMedEWA2018, main = 'EWA by Team 2018', ylab = 'Estimated Wins Added (EWA)', las = 3)
#boxplot(mrg18$Salary / 1000000 ~ mrg18$Tm)
byMedSal2018 <- with(mrg18, reorder(Tm, Salary, median))
boxplot(mrg18$Salary / 1000000 ~ byMedSal2018, main = 'Salary by Team 2018 in Millions US$', ylab = 'Salary in Millions US$', las = 3)

##Look at position
#EWA by postion for 2017
boxplot(mrg17$EWA ~ mrg17$Position)
#Salary by position for 2017
boxplot(mrg17$Salary ~ mrg17$Position)
#EWA by postion for 2018
boxplot(mrg18$EWA ~ mrg18$Position)
#Salary by position for 2017
boxplot(mrg18$Salary ~ mrg18$Position)


#Some values of EWA are 0. This will cause a divide by zero error which R
#handles by returning INF. .01 will be used to replace 0. This is small enough
#to not cause an issue
mrg17$EWA[mrg17$EWA == 0] <- .1
mrg18$EWA[mrg18$EWA == 0] <- .1

mrg17$wincost <- mrg17$Salary / mrg17$EWA
byMedWincost2017 <- with(mrg17, reorder(Tm, wincost, median))
boxplot(mrg17$wincost / 1000000 ~ byMedWincost2017, las = 3, main = "wincost by team 2017 in Millions US$", ylab = "wincost in Millions US$")
mrg18$wincost <- mrg18$Salary / mrg18$EWA
byMedWincost2018 <- with(mrg18, reorder(Tm, wincost, median))
boxplot(mrg18$wincost / 1000000 ~ byMedWincost2018, las = 3, main = "wincost by team 2018  in Millions US$", ylab = "wincost in Millions US$")

#############Good to here
tm17 <- select(mrg17, wincost, Salary, Player, Tm, PosNum.x, Position, PER, VA, EWA)
tm18 <- select(mrg18, wincost, Salary, Player, Tm, PosNum.x, Position, PER, VA, EWA)

ggplot(data = tm17) +
  geom_point(mapping = aes(x = EWA, y = Salary / 1000000, color = Position)) +
  labs(title = "EWA vs. Salary for 2017", y = "Salary in Millions US$")
ggplot(data = tm18) +
  geom_point(mapping = aes(x = EWA, y = Salary / 1000000, color = Position)) +
  labs(title = "EWA vs. Salary for 2018",  y = "Salary in Millions US$")
#Create a boxplot of the wincost
outwincost <- boxplot(tm17$wincost)
#Identify outliers
sort(outwincost$out)

xx <- filter(tm17, wincost >= 7391304 | wincost <= -3833510)
xy <- merge(xx, Playoffs2017, by.x="Tm", by.y="TeamName")

#Mean for wincost by postion
WCbyPos17 <- tapply(tm17$wincost, tm17$Position, mean)

#Create a new dataframe with the team names
train <- as.data.frame(sort(unique(tm17$Tm)))
test <- as.data.frame(sort(unique(tm18$Tm)))
#Rename the column
names(train) <- "Tm"
names(test) <- "Tm"

train <- createDataForSVM(tm17, train)
test <- createDataForSVM(tm18, test)

train <- merge(train, Playoffs2017, by.x = "Tm", by.y = "TeamName")
test <- merge(test, Playoffs2018, by.x = "Tm", by.y = "TeamName")

ggplot(data = train) +
  geom_point(mapping = aes(x = EWATot, y = SalTot / 1000000, color = Playoffs)) +
  labs(title = "Team EWA vs. Team Salary for 2016-17 Season", y = "Team Salary in Millions US$", x = "Cummulative Team EWA")
ggplot(data = test) +
  geom_point(mapping = aes(x = EWATot, y = SalTot / 1000000, color = Playoffs)) +
  labs(title = "Team EWA vs. Team Salary for 2017-18 Season", y = "Team Salary in Millions US$", x = "Cummulative Team EWA")

#SVM time
#model <- svm(Playoffs ~ EWATot + SalTot, data = train, type = "C-classification", kernel = 'polynomial')
#preds <- predict(model, test)
#m <- svm(EWATot ~ SalTot, data  = train, cost = .1, gamma = 1, type = "C-classification", kernel = 'polynomial')
model <- svm(train$EWATot + train$SalTot, train$Playoffs, cost = .1, gamma = 1, type = "C-classification", kernel = 'polynomial')
preds <- predict(model, test$EWATot + test$SalTot)
table(preds)
test$preds <- preds
table(test$preds, test$Playoffs)
results <- confusionMatrix(data = test$preds, reference = test$Playoffs, positive = '1')
print(results)

#Plot without hyperplanes
ggplot(data = test) +
  geom_point(mapping = aes(x = SalTot / 1000000, y = EWATot, color = Playoffs, shape = preds)) +
  labs(title = "Team EWA vs. Team Salary with Playoffs and Predictions", x = "Team Salary in Millions US$", y = "Cummulative Team EWA")


grid <- expand.grid(seq(min(test[, 3]), max(test[, 3]),length.out=30),                                                                                                         
                    seq(min(test[, 4]), max(test[, 4]),length.out=30))

names(grid) <- names(test)[3:4]
preds2 <- predict(model, grid)
df <- data.frame(grid, preds)
cols <- c('1' = 'red', '-1' = 'black')
tiles <- c('1' = 'magenta', '-1' = 'cyan')
shapes <- c('support' = 4, 'notsupport' = 1)
ggplot(test, aes(x = test$SalTot, y = test$EWATot)) + geom_tile(aes(fill = preds)) + 
  scale_fill_manual(values = tiles) +
  geom_point(data = test, aes(color = y, shape = preds), size = 2) +
  scale_color_manual(values = cols) 
  #scale_shape_manual(values = shapes)
obj <- tune.svm(train$EWATot + train$SalTot, train$Playoffs, type = "C-classification", kernel = 'sigmoid', cost = c(.1,.2,.4,1,2))
#fit <- ksvm(EWATot~SalTot,data = train,kernel = "tanhdot", type = "C-svc")
fit <- ksvm(Playoffs ~ .,data = train,kernel = "rbfdot", type = "C-svc")

#KSVAM section that works
#Scale the data for accurate svm results
ktrain<-train
ktest<-test
ktrain$scaleSalTot <- scale(ktrain$SalTot)
ktrain$scaleEWATot <- scale(ktrain$EWATot)
ktest$scaleEWATot <- scale(ktest$EWATot)
ktest$scaleSalTot <- scale(ktest$SalTot)
fit <- ksvm(Playoffs ~ scaleEWATot + scaleSalTot,data = ktrain,kernel = "polydot", scaled = TRUE, type = "C-svc", C = .1)
ktest$preds <- predict(fit, ktest)
confusionMatrix(data = ktest$preds, reference = ktest$Playoffs, positive = '1')
plot(fit, data = ktest)

#plot(SV,pch=19,main="Locations of the support vectors")
