library(nflverse)
library(tidyverse)
library(stringr)
library(dplyr)
library(readr)
#This first section of code will be to explore the data, just to get some ideas of what to do
#make sure your directory call is right
games <- read.csv('NFL/games.csv')
players <- read.csv('NFL/players.csv')
plays <- read.csv('NFL/plays.csv')
tackles <- read.csv('NFL/tackles.csv')
tracking_week_1 <- read.csv('NFL/tracking_week_1.csv')
tracking_week_2 <- read.csv('NFL/tracking_week_2.csv')
tracking_week_3 <- read.csv('NFL/tracking_week_3.csv')
tracking_week_4 <- read.csv('NFL/tracking_week_4.csv')
tracking_week_5 <- read.csv('NFL/tracking_week_5.csv')
tracking_week_6 <- read.csv('NFL/tracking_week_6.csv')
tracking_week_7 <- read.csv('NFL/tracking_week_7.csv')
tracking_week_8 <- read.csv('NFL/tracking_week_8.csv')
tracking_week_9 <- read.csv('NFL/tracking_week_9.csv')

data<-rbind(tracking_week_1, tracking_week_2, tracking_week_3, tracking_week_4, tracking_week_5, tracking_week_6,
      tracking_week_7, tracking_week_8, tracking_week_9)
#created a player data frame with tackling statistics
player_tackles <- tackles %>% group_by(nflId) %>% summarize(Ptackles = sum(tackle), Passists = sum(assist), PforcedFumble = sum(forcedFumble), PmissedTackles = sum(pff_missedTackle))
player_tackles['tacklePercentage'] <- ((player_tackles[c('Ptackles')]))/((player_tackles[c('Ptackles')])+(player_tackles[c('PmissedTackles')]))

player_tackles <- merge(player_tackles, players, by=c('nflId'))

#created data frame that takes the tracking data at the instance of a tackle
week_1_tackles <- tracking_week_1 %>% filter(event == 'tackle')


#created a data frame that merges the play by play data with the instance of a tackle in week 1
tackles_pbp_1 <- merge(week_1_tackles, plays, by=c('playId', 'gameId'))

#I don't think we need these columns, there are probably more we can drop. The less fat the better
clean_tackles_pbp_1 <- subset(tackles_pbp_1, select = -c(yardlineNumber, foulName1, foulName2, foulNFLId1, foulNFLId2, jerseyNumber))

#remove some now unneeded tables to clean up global environment
rm(tracking_week_1, week_1_tackles, tackles_pbp_1)

#can repeat this process for every week to make a clean version of it for every week (doing so below)
week_2_tackles <- tracking_week_2 %>% filter(event == 'tackle')
tackles_pbp_2 <- merge(week_2_tackles, plays, by=c('playId', 'gameId'))
clean_tackles_pbp_2 <- subset(tackles_pbp_2, select = -c(yardlineNumber, foulName1, foulName2, foulNFLId1, foulNFLId2, jerseyNumber))
rm(tracking_week_2, week_2_tackles, tackles_pbp_2)

week_3_tackles <- tracking_week_3 %>% filter(event == 'tackle')
tackles_pbp_3 <- merge(week_3_tackles, plays, by=c('playId', 'gameId'))
clean_tackles_pbp_3 <- subset(tackles_pbp_3, select = -c(yardlineNumber, foulName1, foulName2, foulNFLId1, foulNFLId2, jerseyNumber))
rm(tracking_week_3, week_3_tackles, tackles_pbp_3)

week_4_tackles <- tracking_week_4 %>% filter(event == 'tackle')
tackles_pbp_4 <- merge(week_4_tackles, plays, by=c('playId', 'gameId'))
clean_tackles_pbp_4 <- subset(tackles_pbp_4, select = -c(yardlineNumber, foulName1, foulName2, foulNFLId1, foulNFLId2, jerseyNumber))
rm(tracking_week_4, week_4_tackles, tackles_pbp_4)

week_5_tackles <- tracking_week_5 %>% filter(event == 'tackle')
tackles_pbp_5 <- merge(week_5_tackles, plays, by=c('playId', 'gameId'))
clean_tackles_pbp_5 <- subset(tackles_pbp_5, select = -c(yardlineNumber, foulName1, foulName2, foulNFLId1, foulNFLId2, jerseyNumber))
rm(tracking_week_5, week_5_tackles, tackles_pbp_5)

week_6_tackles <- tracking_week_6 %>% filter(event == 'tackle')
tackles_pbp_6 <- merge(week_6_tackles, plays, by=c('playId', 'gameId'))
clean_tackles_pbp_6 <- subset(tackles_pbp_6, select = -c(yardlineNumber, foulName1, foulName2, foulNFLId1, foulNFLId2, jerseyNumber))
rm(tracking_week_6, week_6_tackles, tackles_pbp_6)

week_7_tackles <- tracking_week_7 %>% filter(event == 'tackle')
tackles_pbp_7 <- merge(week_7_tackles, plays, by=c('playId', 'gameId'))
clean_tackles_pbp_7 <- subset(tackles_pbp_7, select = -c(yardlineNumber, foulName1, foulName2, foulNFLId1, foulNFLId2, jerseyNumber))
rm(tracking_week_7, week_7_tackles, tackles_pbp_7)

week_8_tackles <- tracking_week_8 %>% filter(event == 'tackle')
tackles_pbp_8 <- merge(week_8_tackles, plays, by=c('playId', 'gameId'))
clean_tackles_pbp_8 <- subset(tackles_pbp_8, select = -c(yardlineNumber, foulName1, foulName2, foulNFLId1, foulNFLId2, jerseyNumber))
rm(tracking_week_8, week_8_tackles, tackles_pbp_8)

week_9_tackles <- tracking_week_9 %>% filter(event == 'tackle')
tackles_pbp_9 <- merge(week_9_tackles, plays, by=c('playId', 'gameId'))
clean_tackles_pbp_9 <- subset(tackles_pbp_9, select = -c(yardlineNumber, foulName1, foulName2, foulNFLId1, foulNFLId2, jerseyNumber))
rm(tracking_week_9, week_9_tackles, tackles_pbp_9)
#just did the copy and pasting plus adjusting so its done, idk if its totally necessary

#making one large DF
all_weeks <- rbind(clean_tackles_pbp_9,clean_tackles_pbp_8,clean_tackles_pbp_7,clean_tackles_pbp_6,clean_tackles_pbp_5,clean_tackles_pbp_4,clean_tackles_pbp_3,clean_tackles_pbp_2,clean_tackles_pbp_1)
rm(clean_tackles_pbp_9,clean_tackles_pbp_8,clean_tackles_pbp_7,clean_tackles_pbp_6,clean_tackles_pbp_5,clean_tackles_pbp_4,clean_tackles_pbp_3,clean_tackles_pbp_2,clean_tackles_pbp_1)

#code to create a clean df with all instance of a tackle with tackler and ballcarrier

clean_tackles_all <- merge(all_weeks, tackles, by=c('playId', 'gameId','nflId'),all=TRUE)
clean_tackles_all <- subset(clean_tackles_all, select = -c(forcedFumble,pff_missedTackle,event,frameId,playDirection,yardlineSide,absoluteYardlineNumber,offenseFormation))
clean_tackles_all <- subset(clean_tackles_all, select = -c(preSnapHomeTeamWinProbability,preSnapVisitorTeamWinProbability,homeTeamWinProbabilityAdded,visitorTeamWinProbilityAdded,defendersInTheBox, passLength, prePenaltyPlayResult, penaltyYards, preSnapHomeScore, preSnapVisitorScore))
clean_tackles_all <- subset(clean_tackles_all, select = -c(playDescription))
rm(all_weeks)

#next step is a function that finds who the next tackler would be
#drop all plays nullified
#drop last 2 minutes of each half
#drop any sack
clean_tackles_all <- clean_tackles_all%>%filter(playNullifiedByPenalty!='Y')
clean_tackles_all <- clean_tackles_all%>%filter(passResult!='S')
clean_tackles_all <- subset(clean_tackles_all, select = -c(playNullifiedByPenalty,passResult, passProbability))


clean_tackles_all <- clean_tackles_all %>%
  filter(((as.numeric(str_sub(gameClock, 1, 1)) >= 2)&(quarter==2))|(((as.numeric(str_sub(gameClock, 1, 1)))&(quarter=4))))

library(data.table)