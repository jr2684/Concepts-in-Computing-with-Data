##################################################
## Project: Data Wrangling and Visualization
## 
## Script purpose: This script will be used to 
## preprocess the csv file nba-2018.csv. The 
## endgoal will be the establishment of a ranking
## function. In order to calculate the ranking, we
## first require data that is in the correct 
## format. We will use this script to perform the
## necessary preprocessing
##
## Date: 04.03.2019
## Author: Josh Reinheimer
##################################################

df <- read.csv("../data/nba2018.csv", stringsAsFactors = FALSE)
##### Converting the "R" character to zero #####

df$experience[df$experience == "R"] <- 0
df$experience <- as.integer(df$experience)

##### convert salary #####
df$salary <- df$salary/1000000

##### change position to factor #####
df$position <- factor(df$position,levels = c('C', 'PF', 'PG', 'SF', 'SG'))
levels(df$position) <- c("center", "power_fwd", "point_guard", "small_fwd", "shoot_guard")

##### Adding new variables #####
library(dplyr)
##### adding missed_fg, missed_ft, rebounds, and efficiency to df #####
df <- mutate(df, missed_fg = field_goals_atts - field_goals)
df <- mutate(df, missed_ft = (points3_atts - points3)-(points2_atts-points2))
df <- mutate(df, rebounds = off_rebounds + def_rebounds)
df <- mutate(df, efficiency = (points + rebounds + assists + steals + blocks - missed_fg - missed_ft - turnovers)/games)

##### sending to textfile #####
sink("../output/efficiency-summary.txt")
summary(df$efficiency)
sink()

##### Creating nba2018-teams.csv #####
sink("../output/teams_summary.txt")
n <- length(unique(df$team))
teams <- data.frame(team = character(n),
                    experience = double(n),
                    salary = double(n),
                    points3 = integer(n),
                    points2 = integer(n),
                    points1 = integer(n),
                    points = integer(n),
                    off_rebounds = integer(n),
                    def_rebounds = integer(n),
                    assists = integer(n),
                    steals = integer(n),
                    blocks = integer(n),
                    turnovers = integer(n),
                    fouls = integer(n),
                    efficiency = double(n))
columns = c("experience", "salary", "points3",
            "points2", "points1", "points", "off_rebounds",
            "def_rebounds", "assists", "steals", "blocks",
            "turnovers", "fouls", "efficiency")
teams$team <- unique(df$team)
for(i in 1:length(unique(df$team))){
  for(j in columns){
    teams[i,j] <- sum(df[[j]][df$team == unique(df$team)[i]])
  }
}
sink()
write.csv(teams, "../data/nba2018-teams.csv")
