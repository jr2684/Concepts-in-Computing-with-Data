library(XML)
library(xml2)
library(rvest)
library(magrittr)
library(stringr)

# Assemble url (so it fits on screen)
basket <- "https://www.basketball-reference.com"
gsw <- "/teams/GSW/2017.html"
gsw_url <- paste0(basket, gsw)

# download HTML file to your working directory
download.file(gsw_url, 'gsw-roster-2017.html')

# Read GSW Roster html table
gsw_roster <- readHTMLTable('gsw-roster-2017.html')

nba_html <- paste0(basket, "/leagues/NBA_2017.html")
xml_doc <- read_html(nba_html)
xml_text <- xml_doc %>% html_text()

#extracting content of h2 nodes
h2_headings <- xml_doc %>% html_nodes("h2") %>% html_text()

#extracting_various values
h1_headings <- xml_doc %>% html_nodes("h1") %>% html_text()
strong <- xml_doc %>% html_nodes("strong") %>% html_text()
button <- xml_doc %>% html_nodes("button") %>% html_text()

# getting listheads
listheads <- xml_doc %>% html_nodes(xpath = '//p[@class="listhead"]') %>%
  html_text()
#extracting a tags
a <- xml_doc %>% html_nodes(xpath = '//ul[@class = ""]/li/a')

#extracting href attributes
xml_tables <- xml_doc %>%
  html_nodes("table") %>%
  extract(1:2)
a_nodes <- xml_tables %>% 
  html_nodes("a") %>%
  html_text()
href <- xml_tables %>%
  html_nodes("a") %>%
  html_attr("href")
abbreviations <- str_extract(string = href, pattern = "[A-Z]{3}")
files <- paste0(abbreviations,"-roster-2017.csv")
teams <- paste0("/teams/",abbreviations,"/2017.html")
teams_url <- paste0(basket,teams)
for(i in 1:length(teams_url)){
  temp <- html_table(read_html(teams_url[i]))
  write.csv(temp,paste0("../data/",abbreviations[i],"-roster-2017.csv"))
}

# creating a global file
# i want to create an empty master dataframe, read in the first file, append the abbreviation to it, and do this with every file
# in the data folder

# since the dataframe has to be initiliazed, the following 4 lines are used
# This is simply due to the nature of creating a dataframe. Otherwise, it would have been folded
# into the for loop
myFiles <- list.files("../data")
master <- read.csv(paste0("../data/",myFiles[1]))
team_abbreviation <- str_extract(myFiles[1], pattern = "[A-Z]{3}")
master$Team <- team_abbreviation[1]
for(i in myFiles[2:length(myFiles)]){
  temp <- read.csv(paste0("../data/",i))
  team_abbreviation <- str_extract(i, pattern = "[A-Z]{3}")
  temp$Team <- team_abbreviation
  master <- rbind(master,temp)
}
# writing the dataframe to file
write.csv(master,paste0("../data/","nba-rosters-2017.csv"))