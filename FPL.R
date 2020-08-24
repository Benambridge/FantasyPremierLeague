#This is just for converting the PDF from the FLP site into a .csv file. It's all commented out as I'm supplying the .csv file. So no packages are needed.
#install.packages("pdftables")
#library(pdftables)
#convert_pdf("FPL.pdf", "FPL.csv", format = "csv", api_key = "8e2v78p4ja6y" )
#convert_pdf('test/index.pdf', output_file = NULL, format = "xlsx-single", message = TRUE, api_key = "8e2v78p4ja6y") # More APIs from https://pdftables.com/pdf-to-excel-api

### Increase the maximum printed output length in the R console (so we can see all the teams)
options(max.print=1000000)

### Read the file with the players, points, positions and clubs
P=read.csv("FPL.csv", header=T)
P=P[order(-P$Points),] # Sort the players in descending order of the points they scored last seasonP

### Exclude players with score of zero
P=subset(P, Points>72)

### Make subsets for each position
ALL_GKP=subset(P, Position=="GKP")
ALL_DEF=subset(P, Position=="DEF")
ALL_MID=subset(P, Position=="MID")
ALL_FWD=subset(P, Position=="FWD")

### How many (non-zero-scoring) players are there in each position?
length(ALL_GKP[,1])
length(ALL_DEF[,1])
length(ALL_MID[,1])
length(ALL_FWD[,1])

### set up the empty data frame to hold the data
Master=data.frame("GK1"=character(),"GK2"=character(),"DEF1"=as.character(),"DEF2"=as.character(),"DEF3"=as.character(),"DEF4"=as.character(),"DEF5"=as.character(),"MID1"=as.character(),"MID2"=as.character(),"MID3"=as.character(),"MID4"=as.character(),"MID5"=as.character(),"FWD1"=as.character(),"FWD2"=as.character(),"FWD3"=as.character(),"Points"=as.integer(),"OK"=as.logical())


### Start the clock
ptm = proc.time()

#It does about 2,000 teams in a minute
2000*60 #120,000 teams in an hour
2000*60*24 #2.9m million teams in 24 hours
#So let's try for 3m teams!

for (i in 1:3000000) {

### Randomly select the correct number of players in each position to create the squad
GKP=ALL_GKP[sample(nrow(ALL_GKP), 2, replace=FALSE),]
DEF=ALL_DEF[sample(nrow(ALL_DEF), 5, replace=FALSE),]
MID=ALL_MID[sample(nrow(ALL_MID), 5, replace=FALSE),]
FWD=ALL_FWD[sample(nrow(ALL_FWD), 3, replace=FALSE),]
Squad=rbind(GKP,DEF,MID,FWD)
Squad #Print the squad

### Evaluate the squad
cost=sum(Squad$Cost, na.rm = FALSE) # Get the total cost of the squad
points=sum(Squad$Points, na.rm = FALSE) # Get the points for the squad

### Check the squad contains no more than 3 players from any one team!
check=as.data.frame(table(Squad$Team))
largest=max(check$Freq)

# Check the squad passes both cost and club-limit tests at once
AllOK=(cost<=100)&(largest<=3)

# Fit the whole squad in one row by tranposing (t)
Transpose=as.data.frame(t(Squad))
Transpose=Transpose["Player",]
colnames(Transpose)=c("GK1","GK2","DEF1","DEF2","DEF3","DEF4","DEF5","MID1","MID2","MID3","MID4","MID5","FWD1","FWD2","FWD3")
Transpose$Points=points # Add the total number of points for the squad
Transpose$Cost=cost # Add the total cost for the squad
Transpose$OK=AllOK # Add - Does the team pass the cost and 3-per-club test
Transpose=subset(Transpose, OK==TRUE) # Exclude any team that violates these rules
Master=rbind(Master,Transpose) # Add the current team to the master dataset
Master = Master[order(-Master$Points),] # Sort the master dataset in descending order by (last season's) points
Master=Master[c(1:1000),] # Keep only the Top 1000 teams and discard the rest

p=print(head(Master,10)) # Create a subset of the Top 10 current teams (for looking at in R Console)
write.csv(Master, "Top1000.csv") # Save to a CSV file the current Top 1000 teams
write.csv(Master, "Top1000_Backup1.csv") # And create a backup of this (in case one gets corrupted by a crash etc.)
write.csv(Master, "Top1000_Backup2.csv") # And a second backup
print(p) # Print (in the R Console) the current Top 10 teams
}

# Stop the clock (when all the teams have been evaulated)
proc.time() - ptm



