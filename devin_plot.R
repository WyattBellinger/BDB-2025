setwd("/Users/devin/Desktop/Stat405/final_project")
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

data <- read.csv('nfl_data.csv')


head(data)


player_speed <- function(name){
  
  if (!(name %in% data$displayName)){
    return(FALSE)
  }
  player_name <- data[data$displayName == name, ]
  play_id <- sample(player_name$playId, 1)
  player_play <- player_name[player_name$playId == play_id, ]
  
  
  plot(player_play$frameId, player_play$s, type = "l",
       main = paste(name, "Play Speed Over Time"), 
       xlab = "Time Since Start of Play (Tenths of a Second)",
       ylab = "Speed (yards/second)", 
       col = "blue",           
       lwd = 2,               
       cex.main = 1.5,         
       cex.lab = 1.2,         
       cex.axis = 1.1,         
       font.main = 2,          
       las = 1,                
       bty = "l")              
  
  
  grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
  

  
}

player_speed("Tom Brady")

player_name <- data[data$displayName == "Jalen Hurts", ]

tyreek <- data[data$displayName == "Tyreek Hill", ]
play_id <- sample(tyreek$playId, 1)
tyreek_play <- tyreek[tyreek$playId == play_id, ]


plot(tyreek_play$frameId, tyreek_play$s, type = "l", 
     main = "Tyreek Hill Play Speed Over Time", 
     xlab = "Time Since Start of Play in 10th of a Second",
     ylab = "Speed")


