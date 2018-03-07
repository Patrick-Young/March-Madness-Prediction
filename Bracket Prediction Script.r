###############################################################################
########################## Bracket Prediction #################################
###############################################################################
setwd("C:/Users/Patrick Young/Google Drive/Grad School/Merkle")
# Import Data
teamdata <- read.csv("TeamMonthTotal.csv")
clemsondat <- subset(teamdata, Team == "clemson")
team.names <- unique(teamdata$Team)

############################## Explore Data ####################################
# Clemson win %
mean(clemsondat$Win)
# Clemson season averages
colMeans(clemsondat[,-(1:2)])

team.stats <- data.frame()
for(i in team.names){
    temp.teamdat <- subset(teamdata, Team == i)
    team.avgs <- colMeans(temp.teamdat[,-(1:2)])
    team.stats <- rbind(team.stats, team.avgs)
}
row.names(team.stats) <- team.names
colnames(team.stats) <- colnames(teamdata[,-(1:2)])

# have each row be the difference between team 1's season stats and team 2's 
# season stats and then a win or loss outcome
gamedata <- data.frame()
team.names.all <- teamdata$Team
for(i in seq(1,nrow(teamdata),2)){
    temp.difs <- team.stats[team.names.all[i],] - team.stats[team.names.all[i+1],]
    temp.difs <- c(team.stats[i,"Win."],temp.difs)
    gamedata <- rbind(gamedata,temp.difs)
}

############################## Build Model ####################################

# classification model with probabilites 0 - 1 based on stats?
# how do you factor in SOS?
# predict based on seeds (differential?), conference, avg. stats, win %, 



########################### Import Bracket ####################################

# len 68 vector in order of overall seeds?
# or consecutive teams play each other?



########################### Predict Games ######################################

# predict round 1
# pull winners 
# predict round 2....