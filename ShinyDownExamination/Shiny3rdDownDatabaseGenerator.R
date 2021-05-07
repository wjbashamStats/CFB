library(tidyverse)
library(stringr)
library(purrr)
library(cfbfastR)
library(tictoc)
library(teamcolors)

#Download PBP Data from 2020 season
tictoc::tic()
pbp <- data.frame()
seasons <- 2020

progressr::with_progress({
  future::plan("multisession")
  pbp <- cfbfastR::load_cfb_pbp(seasons)
})
tictoc::toc()

#Summarize Offense look at offense
AllDownSummaryOffense <- pbp %>% filter(rush == 1 | pass == 1) %>% #all rushes and passes
  group_by(pos_team,down) %>% #group by school on offense
  summarize(ypp.rush = mean(yards_gained[rush==1],na.rm = T), #calc the mean of the yards gained per rush down
            ypp.pass = mean(yards_gained[pass==1],na.rm = T),#calc the mean of the yards gained per pass down
            ypp.overall = mean(yards_gained,na.rm = T),
            epa.rush = mean(EPA[rush==1],na.rm = T), #calc the mean of the yards gained per rush down
            epa.pass = mean(EPA[pass==1],na.rm = TRUE),#calc the mean of the yards gained per pass down
            epa.overall = mean(EPA,na.rm = TRUE),
            avg.ytg.run = mean(distance[rush==1],na.rm = T),#calc the mean of yards to go when they run
            avg.ytg.pass = mean(distance[pass==1],na.rm = T),
            avg.ytg.overall = mean(distance,na.rm=T),#calc the mean of yards to go when they run
            num.plays.run = length(pos_team[rush==1]),
            num.plays.pass = length(pos_team[pass==1]),
            num.plays = length(pos_team),
            run.play.frequency = length(pos_team[rush==1])/length(pos_team),
            pass.play.frequency = length(pos_team[pass==1])/length(pos_team)
            )
print(AllDownSummaryOffense) %>% filter(pos_team == 'Clemson')
AllDownSummaryOffense$OffenseOrDefense <- "Offense"
#Summarize Defense
AllDownSummaryDefense <- pbp %>% filter(rush == 1 | pass == 1) %>% #all rushes and passes
  group_by(def_pos_team,down) %>% #group by school on Defense
  summarize(ypp.rush = mean(yards_gained[rush==1],na.rm = T), #calc the mean of the yards gained per rush down
            ypp.pass = mean(yards_gained[pass==1],na.rm = T),#calc the mean of the yards gained per pass down
            ypp.overall = mean(yards_gained,na.rm = T),
            epa.rush = mean(def_EPA[rush==1],na.rm = T), #calc the mean of the yards gained per rush down
            epa.pass = mean(def_EPA[pass==1],na.rm = TRUE),#calc the mean of the yards gained per pass down
            epa.overall = mean(def_EPA,na.rm = TRUE),
            avg.ytg.run = mean(distance[rush==1],na.rm = T),#calc the mean of yards to go when they run
            avg.ytg.pass = mean(distance[pass==1],na.rm = T),
            avg.ytg.overall = mean(distance,na.rm=T),#calc the mean of yards to go when they run
            num.plays.run = length(pos_team[rush==1]),
            num.plays.pass = length(pos_team[pass==1]),
            num.plays = length(pos_team),
            run.play.frequency = length(pos_team[rush==1])/length(pos_team),
            pass.play.frequency = length(pos_team[pass==1])/length(pos_team)) 
AllDownSummaryDefense$OffenseOrDefense <- "Defense"


#Rename first column in both dataframes this to have one singular variable name
colnames(AllDownSummaryOffense)[1] <- "team"
colnames(AllDownSummaryDefense)[1] <- "team"

#combine them together
AllDownSummary<-rbind(AllDownSummaryOffense,AllDownSummaryDefense)

#Rename the downs to look nice on graph
AllDownSummary$down[AllDownSummary$down == "1"] <- "1st"
AllDownSummary$down[AllDownSummary$down == "2"] <- "2nd"
AllDownSummary$down[AllDownSummary$down == "3"] <- "3rd"
AllDownSummary$down[AllDownSummary$down == "4"] <- "4th"

#fix San Jose State so it matches the csv
#NOTE IF YOU ARE USING collegefootballdata.com's csv file delete this line!!!!
AllDownSummary$team[AllDownSummary$team == "San José State"]<-"San Jose State"

#Rename the Columns
names(AllDownSummary)[3]<-'YPP When Playcall is a Run'
names(AllDownSummary)[4]<-'YPP When Playcall is a Pass'
names(AllDownSummary)[5]<-'YPP When Playcall is a Run or Pass'
names(AllDownSummary)[6]<-'Avg EPA Gained When Playcall is a Run'
names(AllDownSummary)[7]<-'Avg EPA Gained When Playcall is a Pass'
names(AllDownSummary)[8]<-'Avg EPA Gained Run or Pass'
names(AllDownSummary)[9]<-'Avg YTG When Playcall is a Run'
names(AllDownSummary)[10]<-'Avg YTG When Playcall is a Pass'
names(AllDownSummary)[11]<-'Avg YTG Run or Pass'
names(AllDownSummary)[12]<-'Snaps when Playcall is a Run'
names(AllDownSummary)[13]<-'Snaps when Playcall is a Pass'
names(AllDownSummary)[14]<-'Snaps'
names(AllDownSummary)[15]<-'Frequency of Runs'
names(AllDownSummary)[16]<-'Frequency of Passes'
#Remove NAs (like 3 teams had 4th down NAs)
AllDownSummary <- data.frame(lapply(AllDownSummary, function(x) {
                    gsub("NA", 0, x)
                }))
AllDownSummary <- data.frame(lapply(AllDownSummary, function(x) {
    gsub("NaN", 0, x)
}))


#join on this file. Replace this with your own path. This file is available for download
cfblogos <- read.csv("/Users/walkerbasham/Desktop/NewFballAnalytics/Shiny20203rdDown/2021CFB3rdDownExamination/appv2/Data/logoLinks.csv") %>% select(school, conference, color, alt_color, logo) #we want to use this as the conferences in Teamcolors are a little different
chartdata1 <- AllDownSummary %>% left_join(cfblogos, by = c("team" = "school"))
chartdata1<-chartdata1[!(chartdata1$conference==""),]#Non FBS teams filtered out
#Some teams don't have alt colors. Replace with white
chartdata1$alt_color[chartdata1$alt_color == ""]<-"#FFFFFF"
#finally, write the csv to a location (obviously change this path)
write.csv(chartdata1,"/Users/walkerbasham/Desktop/NewFballAnalytics/Shiny20203rdDown/2021CFB3rdDownExamination/appv2/Data/ProjData3.csv")
