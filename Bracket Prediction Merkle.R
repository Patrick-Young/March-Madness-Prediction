################################################################################
###################### Merkle March Madness Predictions ########################
########################## William and Mary MSBA ###############################
################################################################################
rm(list = ls())
setwd("C:/Users/Patrick Young/Google Drive/Grad School/Merkle/Kaggle")

results <- read.csv("RegularSeasonDetailedResults.csv")

tourneyresults <- read.csv("NCAATourneyCompactResults.csv")
tourneyresults <- tourneyresults[,-c(2,4,6,7,8)]
tourneyresults <- tourneyresults[tourneyresults$Season >= 2003,]

confTresults <- read.csv("ConferenceTourneyGames.csv")
confTresults <- confTresults[confTresults$Season >= 2003,-c(2,3)]
tourneyresults <- rbind(tourneyresults,confTresults)

conferences <- read.csv("TeamConferences.csv", stringsAsFactors = FALSE)
conf2018 <- conferences[conferences$Season == 2018,]
############## Condense Stats down to Season and Team Avgs #####################

season.stats <- data.frame()
for(seasons in unique(results$Season)){
    for(teams in union(results$WTeamID[results$Season == seasons], results$LTeamID[results$Season == seasons])){
        temp.teamdat.w <- subset(results, Season == seasons & WTeamID == teams)
        temp.teamdat.l <- subset(results, Season == seasons & LTeamID == teams)
        team.sums.w <- colSums(temp.teamdat.w[,-c(1,2,3,5,7,8)])
        team.sums.l <- colSums(temp.teamdat.l[,-c(1,2,3,5,7,8)])
        team.sums.l <- team.sums.l[c(2,1,16:28,3:15)]
        append.w.l <- rbind(team.sums.w, team.sums.l)
        team.sums <- colSums(append.w.l)
        numgames <- nrow(temp.teamdat.w) + nrow(temp.teamdat.l)
        team.avgs <- team.sums / numgames
        win.loss <- nrow(temp.teamdat.w) / (nrow(temp.teamdat.l) + nrow(temp.teamdat.w))
        team.avgs <- c(team.avgs, Season = seasons, team = teams, WinLoss = win.loss)
        season.stats <- rbind(season.stats, team.avgs, stringsAsFactors=FALSE)
    }
}
colnames(season.stats) <- names(team.avgs)

# Caclulate opponent win %
for(seasons in unique(results$Season)){
    for(teams in union(results$WTeamID[results$Season == seasons], results$LTeamID[results$Season == seasons])){
        oppWL <- c()
        gamews <- subset(results, Season == seasons & WTeamID == teams)
        gamels <- subset(results, Season == seasons & LTeamID == teams)
        if(nrow(gamews) > 0){
            for(game in 1:nrow(gamews)){
                tempWL <- season.stats[season.stats$Season == seasons & season.stats$team == gamews[game,"LTeamID"],"WinLoss"]
                oppWL <- c(oppWL,tempWL)
            }
        }
        if(nrow(gamels) > 0){
            for(game in 1:nrow(gamels)){
                tempWL <- season.stats[season.stats$Season == seasons & season.stats$team == gamels[game,"WTeamID"],"WinLoss"]
                oppWL <- c(oppWL,tempWL)
                
            }
        }
        season.stats[season.stats$Season == seasons & season.stats$team == teams,"OppWL"] <- mean(oppWL)
    }
}

# calculate opponents opponent win %
for(seasons in unique(results$Season)){
    for(teams in union(results$WTeamID[results$Season == seasons], results$LTeamID[results$Season == seasons])){
        oppWL <- c()
        gamews <- subset(results, Season == seasons & WTeamID == teams)
        gamels <- subset(results, Season == seasons & LTeamID == teams)
        if(nrow(gamews) > 0){
            for(game in 1:nrow(gamews)){
                tempWL <- season.stats[season.stats$Season == seasons & season.stats$team == gamews[game,"LTeamID"],"OppWL"]
                oppWL <- c(oppWL,tempWL)
            }
        }
        if(nrow(gamels) > 0){
            for(game in 1:nrow(gamels)){
                tempWL <- season.stats[season.stats$Season == seasons & season.stats$team == gamels[game,"WTeamID"],"OppWL"]
                oppWL <- c(oppWL,tempWL)
                
            }
        }
        season.stats[season.stats$Season == seasons & season.stats$team == teams,"OppOppWL"] <- mean(oppWL)
    }
}


################ Set up Model Input - 1 Row per tourney game ###################
model.input <- data.frame()
winners <- c()

for(i in 1:nrow(tourneyresults)){
    seasondata <- season.stats[season.stats$Season == tourneyresults[i,"Season"],]
    Wteamstats <- as.numeric(seasondata[seasondata$team == tourneyresults[i,"WTeamID"],])
    Lteamstats <- as.numeric(seasondata[seasondata$team == tourneyresults[i,"LTeamID"],])
    if(tourneyresults[i,"WTeamID"] > tourneyresults[i,"LTeamID"]){
        stats.dif <- Wteamstats - Lteamstats
        winner <- 1
    }
    else{
        stats.dif <- Lteamstats - Wteamstats
        winner <- 0
    }
    winners <- c(winners, winner)
    model.input <- rbind(model.input, stats.dif)
}
colnames(model.input) <- names(seasondata)
model.input$Win <- winners
model.input <- model.input[,-c(29,30)]

################################## Train Model ################################
# make a model based on past season data
# model is each tournament game - includes avg. season stat differential, win %
# opponent winning %, opponents opponents win %,

library(car)
# WFTM and LFTM are aliased - remove
model.input <- model.input[,-c(7,20)]
my.model <- glm(Win ~ ., data = model.input, family = binomial)

summary(my.model)
vif(my.model)

model.pred <- rep(0, nrow(model.input))
model.pred[predict(my.model, type = 'response') > .5] <- 1
mean(model.pred == model.input$Win) #best so far 75.08%
mean(model.pred != model.input$Win)
table(model.pred, model.input$Win)


########################## Import 2018 Data ###################################
reg2018 <- read.csv("AllSeasonsResults.csv", stringsAsFactors = FALSE)
reg2018 <- reg2018[reg2018$Season == 2018,]
seeds2018 <- read.csv("Prelim2018 - TourneySeeds.csv", stringsAsFactors = FALSE)
seeds2018 <- seeds2018[,-1]

bracketoutline <- read.csv("Prelim2018 - BracketOutline.csv", stringsAsFactors = FALSE)
bracketoutline <- bracketoutline[,-1]

addtoseeds <- cbind(Seed = bracketoutline$Slot, TeamID = NA)
seeds2018 <- rbind(seeds2018,addtoseeds)

stats.2018 <- data.frame()
for(teams in union(reg2018$WTeamID, reg2018$LTeamID)){
    temp.teamdat.w <- subset(reg2018, WTeamID == teams)
    temp.teamdat.l <- subset(reg2018, LTeamID == teams)
    team.sums.w <- colSums(temp.teamdat.w[,-c(1,2,3,5,7,8)])
    team.sums.l <- colSums(temp.teamdat.l[,-c(1,2,3,5,7,8)])
    team.sums.l <- team.sums.l[c(2,1,16:28,3:15)]
    append.w.l <- rbind(team.sums.w, team.sums.l)
    team.sums <- colSums(append.w.l)
    numgames <- nrow(temp.teamdat.w) + nrow(temp.teamdat.l)
    team.avgs <- team.sums / numgames
    win.loss <- nrow(temp.teamdat.w) / (nrow(temp.teamdat.l) + nrow(temp.teamdat.w))
    team.avgs <- c(team.avgs, team = teams, WinLoss = win.loss)
    stats.2018 <- rbind(stats.2018, as.numeric(team.avgs))
}
colnames(stats.2018) <- names(team.avgs)

# opponent win %
for(teams in union(reg2018$WTeamID, reg2018$LTeamID)){
    oppWL <- c()
    gamews <- subset(reg2018, WTeamID == teams)
    gamels <- subset(reg2018, LTeamID == teams)
    if(nrow(gamews) > 0){
        for(game in 1:nrow(gamews)){
            tempWL <- stats.2018[stats.2018$team == gamews[game,"LTeamID"],"WinLoss"]
            oppWL <- c(oppWL,tempWL)
        }
    }
    if(nrow(gamels) > 0){
        for(game in 1:nrow(gamels)){
            tempWL <- stats.2018[stats.2018$team == gamels[game,"WTeamID"],"WinLoss"]
            oppWL <- c(oppWL,tempWL)
        }
    }
    stats.2018[stats.2018$team == teams,"OppWL"] <- mean(oppWL)
}

# calculate opponents opponent win %
for(teams in union(reg2018$WTeamID, reg2018$LTeamID)){
    oppWL <- c()
    gamews <- subset(reg2018, WTeamID == teams)
    gamels <- subset(reg2018, LTeamID == teams)
    if(nrow(gamews) > 0){
        for(game in 1:nrow(gamews)){
            tempWL <- stats.2018[stats.2018$team == gamews[game,"LTeamID"],"OppWL"]
            oppWL <- c(oppWL,tempWL)
        }
    }
    if(nrow(gamels) > 0){
        for(game in 1:nrow(gamels)){
            tempWL <- stats.2018[stats.2018$team == gamels[game,"WTeamID"],"OppWL"]
            oppWL <- c(oppWL,tempWL)
            
        }
    }
    stats.2018[stats.2018$team == teams,"OppOppWL"] <- mean(oppWL)
}

tourneyteams <- stats.2018[stats.2018$team %in% seeds2018$TeamID,]

########################### Predict Games ######################################
teams <- read.csv("Teams.csv", stringsAsFactors = FALSE)
teams <- teams[,-c(3,4)]

gameprobs <- c()
gameoutcomes <- data.frame()

for(i in 1:nrow(bracketoutline)){
    team1 <- seeds2018[seeds2018$Seed == bracketoutline[i,2],2]
    team2 <- seeds2018[seeds2018$Seed == bracketoutline[i,3],2]
    team1stats <- tourneyteams[tourneyteams$team == team1,-c(7,20,29)]
    team2stats <- tourneyteams[tourneyteams$team == team2,-c(7,20,29)]
    stats.dif <- team1stats - team2stats
    simgame <- predict(my.model, stats.dif, type = 'response')
    gameprobs <- c(gameprobs,simgame)
    if(simgame > .5){
        seeds2018[seeds2018$Seed == bracketoutline[i,1],2] <- team1
        gameoutcome <- c(teams[teams$TeamID == team1,2],teams[teams$TeamID == team2,2],
                         simgame,teams[teams$TeamID == team1,2])
    }
    else{
        seeds2018[seeds2018$Seed == bracketoutline[i,1],2] <- team2
        gameoutcome <- c(teams[teams$TeamID == team1,2],teams[teams$TeamID == team2,2],
                         as.numeric(simgame),teams[teams$TeamID == team2,2])
    }
    gameoutcomes <- rbind(gameoutcomes, gameoutcome, stringsAsFactors=FALSE)
}
colnames(gameoutcomes) <- c("Team1","Team2","Prob","Winner")
gameoutcomes

#write.csv(gameoutcomes, "GameOutcomes.csv")
