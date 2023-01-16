#####Pre-processing
#Load in datasets
batting <- read.csv(
  "C:\\Users\\schei\\OneDrive\\Data Analytics\\MLB Top 100 Prospects List\\8-10-22\\Data\\batting.csv"
)
pitching <- read.csv(
  "C:\\Users\\schei\\OneDrive\\Data Analytics\\MLB Top 100 Prospects List\\8-10-22\\Data\\pitching.csv"
)
options(scipen=200)

#convert columns to rownames
batting <- tibble::column_to_rownames(batting, "Name")
pitching <- tibble::column_to_rownames(pitching, "Name")

#remove variables irrelevant to my ranking
batting <- batting[, -c(1,3,5,6,19)]
pitching <- pitching[, -c(1,3,5,6,19)]

#floor age
batting$Age <- floor(batting$Age)
pitching$Age <- floor(pitching$Age)

#convert fv to numerical
batting$FV <- gsub('[+]', '', batting$FV)
pitching$FV <- gsub('[+]', '', pitching$FV)
batting$FV <- as.numeric(batting$FV)
pitching$FV <- as.numeric(pitching$FV)

#convert batters' tools to numerical (present/potential)
for(i in 3:6)
{
  x <- substr(batting[,i], 0, 2)
  x <- as.numeric(x)
  y <- substr(batting[,i], 6, 7)
  y <- as.numeric(y)
  z <- x/y
  batting[,i] <- z
}

#convert pitchers' tools to numerical (present/potential)
for(i in 3:7)
{
  pitching[,i][pitching[,i]==""] <- 0000000000 #fill empty cells with 0
  x <- substr(pitching[,i], 0, 2)
  x <- as.numeric(x)
  y <- substr(pitching[,i], 6, 7)
  y <- as.numeric(y)
  z <- x/y
  pitching[,i] <- z
}

#scale everything but position/(role)
batting <- cbind(batting[1], scale(batting[, c(2:14)]))
pitching <- cbind(pitching[1], scale(pitching[, c(2:14)]))

#convert position/(role) to binary matrices
pos <- as.factor(batting$Pos)
pos <- model.matrix(~0 + pos)
batting <- cbind(batting[,-1], pos[,-1])
role <- as.factor(pitching$Pos)
role <- model.matrix(~0 + role)
pitching <- cbind(pitching[,-1], role[,-1])

#####Determine common ratings between pitchers and hitters
#hitter tools
tools <- ((batting[,2]*1) + (batting[,3]*1) +
  (batting[,4]*1) + (batting[,5]*1))/4
batting <- cbind(batting, tools)

#hitter stats
stats <- ((batting[,7]*1) + (batting[,8]*1) +
    (batting[,9]*1) + (batting[,10]*1) + (batting[,11]*1) +
      (batting[,12]*1) + (batting[,13]*1))/7
batting <- cbind(batting, stats)

#hitter position
position <- ((batting[,14]*1) + (batting[,15]*1.5) +
    (batting[,16]*1.25) + (batting[,17]*1.1) + (batting[,18]*1.1) +
      (batting[,19]*1.1) + (batting[,20]*1.75))/7
  #in order of value: SS, 3B, C, OF, 2B, 1B
batting <- cbind(batting, position)

#pitcher tools
  #first convert NAs to 0
pitching[is.na(pitching)] <- 0
tools <- ((pitching[,2]*1) + (pitching[,3]*1) +
    (pitching[,4]*1) + (pitching[,5]*1) + (pitching[,6]*1))/5
pitching <- cbind(pitching, tools)

#pitcher stats
stats <- ((pitching[,8]*1) + (pitching[,9]*1) + (pitching[,10]*1) +
    (pitching[,11]*1) + (pitching[,12]*1) + (pitching[,13]*1))/6
pitching <- cbind(pitching, stats)

#pitcher position
position <- ((pitching[,14]*-0.5) + (pitching[,15]*4))/2
  #starters valued more, SIRPs valued worse
pitching <- cbind(pitching, position)

#####Combine hitters and pitchers
###keep Age,FV,Tools,Stats,Pos
batting <- batting[,-c(2:5, 7:21)]
pitching <- pitching[,-c(2:6, 8:15)]

#make a new dataset for all prospects
prospects <- rbind(batting, pitching)

#remove any players with NAs
prospects <- na.omit(prospects)

# #####Choosing optimal # clusters
# withindist <- 0
# clusterlist <- 1:15
# for(i in clusterlist)
# {
#   mlbCluster <- kmeans(prospects, i)
#   withindist <- c(withindist, mlbCluster$tot.withinss)
# }
# plot(withindist[-1])

#####Run kmeans
prospectTop100 <- kmeans(prospects, 8)

# ####exporting to sep. csvs
# write.csv(prospectTop100$centers, "z_$centers.csv")
# write.csv(prospectTop100$cluster, "z_$cluster.csv")
# write.csv(prospects, "z_prospects.csv")

#####Adding player info to top 100 list
#add in the 100 players and make extra columns, fill in with NA
finalList <- read.csv(
  "C:\\Users\\schei\\OneDrive\\Data Analytics\\MLB Top 100 Prospects List\\8-10-22\\Data\\top100csv.csv"
)
finalList[, "Org"] <- NA
finalList[, "Pos"] <- NA
finalList[, "Current.Level"] <- NA

#reload in datasets
batting2 <- read.csv(
  "C:\\Users\\schei\\OneDrive\\Data Analytics\\MLB Top 100 Prospects List\\8-10-22\\Data\\batting.csv"
)
pitching2 <- read.csv(
  "C:\\Users\\schei\\OneDrive\\Data Analytics\\MLB Top 100 Prospects List\\8-10-22\\Data\\pitching.csv"
)
###run a nested for loop to check if player had been found;
  #if found, add player info to finalList dataframe
for (i in 1:nrow(finalList)) #100 times
{
  found = FALSE
  for (a in 1:nrow(batting2)) #403 times
  {
    if(finalList[i,1] == batting2[a,1]) #if names are equal
    {
      finalList[i,1:4] <- batting2[a,1:4] #fill in info
      found = TRUE
    }
  }
  if(found == FALSE) #if not a hitter
  {
    for (b in 1:nrow(pitching2)) #421 times
    {
      if(finalList[i,1] == pitching2[b,1]) #if names are equal
      {
        finalList[i,1:4] <- pitching2[b,1:4] #fill in info
      }
    }
  }
}
###write updated dataframe to top100csv.csv
write.csv(finalList, "C:\\Users\\schei\\OneDrive\\Data Analytics\\MLB Top 100 Prospects List\\8-10-22\\top100csv.csv")
