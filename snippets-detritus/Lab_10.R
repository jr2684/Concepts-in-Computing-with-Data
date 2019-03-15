library(dplyr)
library(stringr)
library(plotly)

github <- "https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2018/master/"
datafile <- "data/mobile-food-sf.csv"
download.file(paste0(github, datafile), destfile = "mobile-food-sf.csv")

dat <- read.csv('data/mobile-food-sf.csv', stringsAsFactors = FALSE)
