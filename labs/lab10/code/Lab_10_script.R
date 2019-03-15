library(dplyr)
library(readr)
library(stringr)
library(plotly)
library(ggmap)

github <- "https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2018/master/"
datafile <- "data/mobile-food-sf.csv"
download.file(paste0(github, datafile), destfile = "mobile-food-sf.csv")
# importing the data. Remember to set the workings
# directory to where this script is located. If this
# is not done, then the pathway in the following 
# statement will not work.
dat <- read.csv("../data/mobile-food-sf.csv", stringsAsFactors = FALSE)

# plts with "plotly"
day_freqs <- table(dat$DayOfWeekStr)
barplot(day_freqs, border = NA, las = 3)

plot_ly(x = names(day_freqs),
        y = day_freqs,
        type = 'bar')

# Alternative method to creating the frequency table
day_counts <- dat %>%
  select(DayOfWeekStr) %>%
  group_by(DayOfWeekStr) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

plot_ly(day_counts,
        x = ~reorder(DayOfWeekStr, count), 
        y = ~count,
        type = 'bar')

# changing times
time1 <- '10AM'
str_sub(time1, start = 1, end = 2)
times <- c('12PM', '10AM', '9AM', '8AM', '2PM')
time_subset = c()
periods = str_sub(times,start = -2)
# loop to determine the time. Should be replaced with a 
# regex statement
for(i in times){
  if(nchar(i) == 4){
    time_subset <- c(time_subset,str_sub(i, start = 1, end = 2))
  }else{
    time_subset <- c(time_subset,str_sub(i,start = 1, end = 1))
  }
}
hours <- str_replace(times,pattern = 'AM|PM', replacement = '')
periods <-  str_sub(times,start = -2)

plot_ly(x = unique(periods),
        y = table(periods),
        type = 'bar')

#' @title 24 hour clock convertor
#' @description Function takes in a list of times, where
#' each element is given as a 12 hour time with am or
#' pm, and converts that given time to its corresponding
#' value in military time e.g. '8PM' --> 20
#' @param x. list of elements given in the from (integer 1-12).('PM' or 'AM) (vector of character)
#' @return y. Corresponding values in military time (vector of integers)


convert_to_military_time <- function(x){
  start24 <- c()
  for(i in x){
    if(str_sub(i,start = -2) == "PM"){
      start24 <- c(start24,as.numeric(str_replace(i,pattern = 'AM|PM', replacement = ''))+12)
    }else{
      start24 <- c(start24,as.numeric(str_replace(i,pattern = 'AM|PM', replacement = '')))
    } 
  }
  return(start24)
}
dat$start <- convert_to_military_time(dat$starttime)
dat$end <- convert_to_military_time(dat$endtime)
dat$duration <- dat$end - dat$start

##### Latitude and Longitude Coordinates #####
loc1 <- dat$Location[1]
lat_lon <- str_split(str_replace_all(loc1, pattern = '\\)|\\(', replacement = ''), pattern = ',')

locs <- c(
  "(37.7651967350509,-122.416451692902)",
  "(37.7907890558203,-122.402273431333)",
  "(37.7111991003088,-122.394693339395)",
  "(37.7773000262759,-122.394812784799)",
  NA
)
lat_lons <- str_split(str_replace_all(locs, pattern = '\\)|\\(', replacement = ''), pattern = ',')
lats <- as.numeric(lapply(lat_lons,function(x) x[1]))
lons <- as.numeric(lapply(lat_lons,function(x) x[2]))

# in order to convert location to latitude/longitude, we will go through a few steps. First a temporary column
# will be constructed
temp <- dat$Location
# next, the values in temp will be converted to lists
temp <- str_split(str_replace_all(temp, pattern = '\\)|\\(', replacement = ''), pattern = ',')
# next, the Lat and Lon columns will be constructed
Lat <- as.numeric(lapply(temp, function(x) x[1]))
Lon <- as.numeric(lapply(temp, function(x) x[2]))
#finally, the columns are appended to the dataframe
dat$lat <- Lat
dat$lon <- Lon

##### plotting locations on a map #####

#naive option
plot(dat$lon, dat$lat, pch = 19, col = "#77777744")

#scatterplot
plot_ly(x = dat$lon, y = dat$lat, type = 'scatter', mode = 'markers')

##### ggplot #####
dat <- na.omit(dat)
sbbox <- make_bbox(lon = dat$lon, lat = dat$lat, f = .1)
sbbox
sf_map <- get_map(location = sbbox, maptype = "terrain", source = "google")
ggmap(sf_map) + 
  geom_point(data = dat, 
             mapping = aes(x = lon, y = lat), 
             color = "red", alpha = 0.2, size = 1)


##### Specific types of food #####

foods <- dat$optionaltext[1:10]
#use regex to consider both upper and lower case words. Another possibility would be to first casefold the 
# entire dataframe.
burritos <- str_detect(foods, pattern = "(B|b)urrito")
#creating a dataframe that consists only of food trucks that serve burritos
burritos <- dat[str_detect(dat$optionaltext, pattern = "(B|b)urrito") == TRUE,]

ggmap(sf_map) + 
  geom_point(data = burritos, 
             mapping = aes(x = lon, y = lat), 
             color = "blue", alpha = 0.2, size = 1)

##### looking at different types of foods #####


