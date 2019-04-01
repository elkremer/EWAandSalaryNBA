library(ggplot2)

#Read in the data files
sal2016 <- read.csv("C:/Users/Peter/Desktop/Utica/Capstone-680/InputData/2016-17NBASalaries.csv")
stats2016 <- read.csv("C:/Users/Peter/Desktop/Utica/Capstone-680/InputData/2016-17PlayerStatsPer100.csv")
advStats2016 <- read.csv("C:/Users/Peter/Desktop/Utica/Capstone-680/InputData/2016-17AdvancedStats.csv")
EWA2016 <- read.csv("C:/Users/Peter/Desktop/Utica/Capstone-680/InputData/2016-17EWA.csv")
Playoffs2016 <- read.csv("C:/Users/Peter/Desktop/Utica/Capstone-680/InputData/2016-17Playoffs.csv")

#Keep only players who played at least 500 minutes during the season
#(6 minutes per game)
stats2016 <- subset(stats2016, MP > 500)
advStats2016 <- subset(advStats2016, MP > 500)

#Keep only those who have played more than 30 games
stats2016 <- filter(stats2016, Games > 30)

#Remove the rows with TOT for Tm since this represents the Total for players
#with more than one team.
stats2016 <- subset(stats2016, Tm != 'TOT')
advStats2016 <- subset(advStats2016, Tm != 'TOT')

sal2016$Base <- as.numeric(gsub('[$,]', '', sal2016$Base))

cd <- advStats2016[,c(9,10,11,12,13,14,15,16,17,18,19,20,21)]

boxplot(sal2016$Base)
boxplot(EWA2016$EWA)
clusdata <- stats2016[,c(12,13,15,17,21,22,24,25,26,27,28,29)]
clusdata2 <- stats2016[,c(12,17,19,20,21,22,24,25,26,27,29)]
clusdata3 <- stats2016[,c(11,12,18,23,24,25,26,27,29)]
clusdata4 <- stats2016[,c(10,11,18,20,21,24,26,29)]
cd <- advStats2016[,c(3,9,10,11,12,13,15,16,17,18,19,20,21)]

set.seed(8675309) #Set seed for reproducibility
krange <- 5:15 #K range from 5 to 15
trials <- 100  #Number of times to run k means
totw.ss <- integer(length(krange)) #Empty vector to hold points
for (k in krange) {
  tmp.totw.ss <- integer(trials) #Empty vector to hold the trials
  for (x in 1:trials) {
    #tmp <- kmeans(stats2016[,-c(1,2,3,4,5,6,7,8,10,14,16,19,23,31,32)],centers = k, nstart = 25)
    #tmp <- kmeans(stats2016[,c(12,13,15,17,21,22,24,25,26,27,28,29)], centers = k, nstart = 25)
    #tmp <- kmeans(stats2016[,c(31,32)], centers = k, nstart = 25)
    #tmp <- kmeans(stats2016[,c(4,10,11,12,13,14,16,17,18,19,20,21,22,23,25,26,27,28,29,30)], centers = k, nstart = 20)
    tmp <- kmeans(cd, centers = k, nstart = 25)
    tmp.totw.ss[x] <- tmp$tot.withinss
  }
  totw.ss[k-4] <- mean(tmp.totw.ss) #average of the withinss
}

plot(krange, totw.ss, type = "b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")

#Dendogram
t <- stats2016[,c(4,33)]
den <- hclust(dist(t))
plot(den)

od <- kmeans(stats2016[,c(4,31,32)], centers = 8, nstart = 25)
stats2016$newPos <- od$cluster
t <- stats2016[,c(4,33)]
#WorthCluster <- kmeans(stats2016[,-c(1,2,3,4,5,6,7,8,10,14,16,19,23,31,32)], 7, nstart = 25)
#table(wortchCluster$cluster)

mrg <- merge(EWA2016, sal2016)
mrg <- merge(mrg, stats2016, by.x ="Player", by.y="Player")

boxplot(mrg$EWA ~ mrg$Tm)
boxplot(mrg$Base ~ mrg$Tm)
boxplot(mrg$EWA ~ mrg$Position)
boxplot(mrg$Base ~ mrg$Position)

  #Some values of EWA are 0. This will cause a divide by zero error which R
  #handles by returning INF. .01 will be used to replace 0. This is small enough
  #to not cause an issue
mrg$EWA[mrg$EWA == 0] <- .1

mrg$wincost <- mrg$Base / mrg$EWA 
boxplot(mrg$wincost ~ mrg$Tm)

finalData <- select(mrg, wincost, Base, Player, Tm, PosNum.x, Position, PER, VA, EWA)

ggplot(data = finalData) +
  geom_point(mapping = aes(x = EWA, y = Base, color = PosNum.x))

#Create a boxplot of the wincost
outwincost <- boxplot(finalData$wincost)
#Identify outliers
sort(outwincost$out)

xx <- filter(finalData, wincost >= 7391304 | wincost <= -3833510)
xy <- merge(xx, Playoffs2016, by.x="Tm", by.y="TeamName")

#Mean for wincost by postion
WCbyPos <- tapply(finalData$wincost, finalData$Position, mean)

tmData <- as.data.frame(sort(unique(finalData$Tm)))
names(tmData) <- "Tm"
WCTot <- as.vector(tapply(finalData$wincost, finalData$Tm, sum))
EWATot <- as.vector(tapply(finalData$EWA, finalData$Tm, sum))
SalTot <- as.vector(tapply(finalData$Base, finalData$Tm, sum))
#Remove pesky 'TOT' Team value
WCTot <- WCTot[-29]
EWATot <- EWATot[-29]
SalTot <- SalTot[-29]

#Add the new columns
tmData$WCTot <- WCTot
tmData$EWATot <- EWATot
tmData$SalTot <- SalTot
tmData <- merge(tmData, Playoffs2016, by.x = "Tm", by.y = "TeamName")

ggplot(data = tmData) +
  geom_point(mapping = aes(x = EWATot, y = SalTot, color = Playoffs))
#SVM time


#tmData <- as.data.frame(WCbyTeam)
#tmData$EWATot <- as.vector(tapply(finalData$wincost, finalData$Tm, sum))

mu <- sd(finalData$wincost)
