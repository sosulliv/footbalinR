###########################################################
#          NFL Team Play Selection Predictions            #                                                   #
# A simple prediction model that predicts if a team will  #
# run or pass on given play. The model takes play         #
# selection data from all the preceeding games in the     #
# season, considering various factor to determine if a    #
# will run a pass on plays in the team's final game.      #
###########################################################


setwd("~/Documents/football") #Set working directory

#library(rpart)
#library(rattle)
#library(rpart.plot)
#library(RColorBrewer)

#IMPORT FILE
pbp_2015 <- read.csv("~/Documents/football/pbp-2015.csv", stringsAsFactors=FALSE) #read play by file into play by play
#FILTER DATASET FOR PLAYTYPE PASS OR RUSH
pbp_sub_2015<-subset(pbp_2015, PlayType=="PASS" | PlayType=="RUSH")
#FILTER PLAYTYPE SUBSET FOR TEAM AND FILTER OUT LAST GAME PLAYED
pbp_sub_2015_team<-subset(pbp_sub_2015, OffenseTeam=="PHI" & GameId!=2015122600)
#FIT TEAM DATA TO DECISION TREE
fit_team <- rpart(PlayType ~  Quarter + Minute + Down + ToGo + YardLine + Formation, data=pbp_sub_2015_team, method="class")
#PLOT DECISION TREE
fancyRpartPlot(fit_team)
#FILTER PLAYTYPE SUBSET FOR TEAM AND LAST GAME
pbp_sub_2015_team_game<-subset(pbp_sub_2015, OffenseTeam=="PHI" & GameId==2015122600)
#PREDICT PLAY SELECTION FOR LAST GAME
Prediction_team <- predict(fit_team, pbp_sub_2015_team_game, type = "class")
#CREATE DATA FRAME FOR LAST GAME PREDICTIONS
submit_team_game <- data.frame(Play_id = paste(pbp_sub_2015_team_game$GameId, pbp_sub_2015_team_game$Quarter, pbp_sub_2015_team_game$Minute, pbp_sub_2015_team_game$Second), pbp_sub_2015_team_game$PlayType, PlayType_Pred = Prediction_team)
#WRITE OUT RESULTS
write.csv(submit_team_game , file = "playprediction2015team.csv", row.names = FALSE)

