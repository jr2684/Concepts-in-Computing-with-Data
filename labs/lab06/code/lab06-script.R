# Title: Lab 6: data wrangling
# Description: Using a large dataset, we will
#   perform some more data wrangling and practice
#   producing files that will be outputted. In 
#   particular, we will practice producing graphs
#   and dataframes that we want to export. Learning how
#   to have these items be produced in the correct project
#   files is the objective of this lab
# Input(s): data file 'nba2018-players.csv'
# output(s): data file 'clean-data.csv'
# Author: Josh Reinheimer
# Date: 2-27-2019

#necessary packages
library(readr)
library(dplyr)
library(ggplot2)

datafile <- read_csv("../data/nba2018-players.csv")

# create and export a dataframe that consist only
# of players from the Golden State Warriors
warriors <- datafile[datafile$team == "GSW",]
warriors <- warriors[order(warriors$salary),]
write.csv(warriors, file = '../data/warriors.csv',row.names = FALSE)

# create and export a dataframe consisting only
# of players from the Lakers. Players are ordered
# by level of experience
Lakers <- datafile[datafile$team == 'LAL',]
Lakers <- Lakers[order(Lakers$experience, decreasing = TRUE),]
write_csv(Lakers, '../data/Lakers.csv')

# In the following lines, we will be outputting some simple descriptive statistics to
# file. This will be accomplished with the sink function. The statistics will be outputted to the
# output folder
sink(file = '../output/data-structure.txt')
str(datafile)
sink()

sink(file = '../output/summary-warriors.txt')
summary(warriors)
sink()

sink(file = '../output/summary-lakers.txt')
summary(Lakers)
sink()


png(filename = '../images/scatterplot-height-weight.png')
plot(datafile$height,datafile$weight, pch = 20, 
     xlab = 'Height', ylab = 'Weight')
dev.off()

png(filename = '../images/scatterplot-height-weight-resolution.png', res = 100)
plot(datafile$height,datafile$weight, pch = 20, 
     xlab = 'Height', ylab = 'Weight')
dev.off()

jpeg(filename = '../images/histogram-age.jpg', width = 600, height = 400, units = "px")
hist(datafile$age)
dev.off()

pdf(file = "../images/histogram-age.pdf", width = 7, height = 5)
hist(datafile$age)
dev.off()

# Next, a number of plots will be produced using
# the ggplot2 library
gg_points_salary <- ggplot(datafile, aes(x = points, y = salary)) + geom_point()
ggsave("points_salary.pdf",path = "../images", width = 7, height = 5, units = "in" )


gg_ht_wt_positions <- ggplot(datafile, aes(x = height, y = weight)) + geom_point()  + facet_grid(col = vars(position))
ggsave("height_weight_by_position.pdf",path = "../images", width = 6, height = 4, units = "in" )

# using the "%>%" operator

# display the player names of Lakers 'LAL'.
datafile %>% filter(team =="LAL") %>% select(player)

# display the name and salary of GSW point guards 'PG'.
datafile %>% filter(position == "PG") %>% select(c("player", "salary"))

#dislay the name, age, and team, of players with
# more than 10 years of experience, making 10 million dollars or less.
datafile %>% filter(salary <= 10000000 & experience > 10) %>% select(c("player", "age", "team"))

# select the name, team, height, and weight, of 
# rookie players, 20 years old, displaying only 
# the first five occurrences (i.e. rows).
datafile %>% filter(age == 20 & experience == 0) %>% select(c("player", "team", "weight")) %>% head(5)

# create a data frame gsw_mpg of GSW players, that 
# contains variables for player name, experience, 
# and min_per_game (minutes per game), 
# sorted by min_per_game (in descending order).
datafile %>% mutate(min_per_game = minutes/games) %>% select(c('player', 'experience', 'min_per_game')) %>% arrange(desc(min_per_game))

#display the average triple points by team, in ascending order,
#of the bottom-5 teams (worst 3pointer teams).
datafile %>%select(c("team", "points3")) %>% group_by(team) %>%  summarise(avg = mean(points3)) %>% arrange(avg) %>% head(5) 

#obtain the mean and standard deviation of age, for Power Forwards, with 5 and 10 years
# (including) of experience.
datafile %>% filter(position == "PF" & experience == 5 | position == "PF" & experience == 10) %>% select(age) %>% select(age) %>% summarise(mean = mean(age, na.rm = TRUE),std = sd(age, na.rm = TRUE))



