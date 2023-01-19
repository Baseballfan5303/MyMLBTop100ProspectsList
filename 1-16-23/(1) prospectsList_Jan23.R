# set working dir, load in data
setwd("C:\\Users\\schei\\OneDrive\\Data Analytics\\MLB Top 100 Prospects List\\(3) 1-23")
batting <- read.csv('(0) batting-1-23.csv')
pitching <- read.csv('(0) pitching-1-23.csv')

#####Data prep
#convert columns to rownames
batting <- tibble::column_to_rownames(batting, "Name")
pitching <- tibble::column_to_rownames(pitching, "Name")

#remove variables irrelevant to my ranking
irrCols <- c(1,3,4,5,6,19)
batting <- batting[, -irrCols]
pitching <- pitching[, -irrCols]

#convert fv to numerical
batting$FV <- gsub('[+]', '', batting$FV)
pitching$FV <- gsub('[+]', '', pitching$FV)
batting$FV <- as.numeric(batting$FV)
pitching$FV <- as.numeric(pitching$FV)

#convert batters' tools to numerical
for(i in 2:5)
{
  x <- substr(batting[,i], 0, 2) #present
  x <- as.numeric(x)
  y <- substr(batting[,i], 6, 7) #potential
  y <- as.numeric(y) 
  z <- x/y #present/potential
  batting[,i] <- z+x #(pres/pot)+pres
  #players with 60 pres should be rated higher than those with 30
}

#convert pitchers' tools to numerical (present/potential)
for(i in 2:6)
{
  pitching[,i][pitching[,i]==""] <- 0000000000 #fill empty cells with 0
  x <- substr(pitching[,i], 0, 2) #present
  x <- as.numeric(x)
  y <- substr(pitching[,i], 6, 7) #potential
  y <- as.numeric(y)
  z <- x/y #present/potential
  pitching[,i] <- z+x #(pres/pot)+pres
  #players with 60 pres should be rated higher than those with 30
}

#scale everything but position/(role)
batting <- cbind(batting[1], scale(batting[, c(2:13)]))
pitching <- cbind(pitching[1], scale(pitching[, c(2:13)]))

#convert position +/or role to binary matrices
pos <- as.factor(batting$Pos)
pos <- model.matrix(~0 + pos)
batting <- cbind(batting[,-1], pos[,-1])
role <- as.factor(pitching$Pos)
role <- model.matrix(~0 + role)
pitching <- cbind(pitching[,-1], role[,-1])

#####Determine common ratings between pitchers and hitters
#hitter tools
tools <- ((batting[,1]*1) + (batting[,2]*1) +
            (batting[,3]*1) + (batting[,4]*1))/4
batting <- cbind(batting, tools)

#hitter stats
stats <- ((batting[,6]*1) + (batting[,7]*1) +
            (batting[,8]*1) + (batting[,9]*1) + (batting[,10]*1) +
            (batting[,11]*1) + (batting[,12]*1))/7
batting <- cbind(batting, stats)

#hitter position
position <- ((batting[,13]*1) + (batting[,14]*1.25) +
               (batting[,15]*1.5) + (batting[,16]*1.1) + (batting[,17]*1.1) +
               (batting[,18]*1.1) + (batting[,19]*1.75))
#in order of value: SS, C, 3B, OF, 2B, 1B
batting <- cbind(batting, position)

#pitcher tools
#first convert NAs to 0
pitching[is.na(pitching)] <- 0
tools <- ((pitching[,1]*1) + (pitching[,2]*1) +
            (pitching[,3]*1) + (pitching[,4]*1) + (pitching[,5]*1))/5
pitching <- cbind(pitching, tools)

#pitcher stats
stats <- ((pitching[,7]*1) + (pitching[,8]*1) + (pitching[,9]*1) +
            (pitching[,10]*1) + (pitching[,11]*1) + (pitching[,12]*1))/6
pitching <- cbind(pitching, stats)

#pitcher position
position <- ((pitching[,13]*-0.5) + (pitching[,14]*2))
#starters valued more, SIRPs valued worse
pitching <- cbind(pitching, position)

#####Combine hitters and pitchers
###keep FV,Tools,Stats,Pos
batting <- batting[,c(5,20:22)]
pitching <- pitching[,c(6, 15:17)]

#make a new dataset for all prospects
prospects <- rbind(batting, pitching)

#remove any players with NAs
prospects <- na.omit(prospects)

###Prospect score
prospects['Score'] <- prospects[1]+prospects[2]+prospects[3]+prospects[4]
#sort by score desc for final list
prospects <- dplyr::arrange(prospects, desc(Score))

#Convert rownames back to column
prospects <- tibble::rownames_to_column(prospects, 'Name')

#####save and export final list
write.csv(prospects, '(2) prospect-list-sorted.csv')