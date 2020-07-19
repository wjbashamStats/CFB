library(cfbscrapR)
library(tidyverse)
library(dplyr)
library(tidyr)
library(gt)
library(paletteer) # for all the palettes
library(ggExtra)

Linetable<-cfb_betting_lines(year = 2019, line_provider = "Caesars")
print(Linetable)
Linetable$spread <- as.numeric(Linetable$spread)
Linetable$overUnder <- as.numeric(Linetable$overUnder)

#Linetable$homeTeam <- factor(Linetable$homeTeam, ordered = TRUE)
#Linetable$id <- integer(Linetable$id, ordered = TRUE)

#LinetableSorted<-Linetable[order(Linetable$id),]
head(Linetable)

Linetable$upset<-0
Linetable$upset[Linetable$homeScore > Linetable$awayScore & Linetable$spread > 0] <- 1
Linetable$upset[Linetable$awayScore > Linetable$homeScore & Linetable$spread < 0] <- 1

Linetable$scoredifference <- abs(Linetable$homeScore-Linetable$awayScore)

upsets<- filter(Linetable, upset==1, spread > 3 | spread < -3)

str(upsets)

head(upsets)
upsets<-upsets[order(-upsets$scoredifference),]
upsetsbySpread<-upsets[order(-upsets$spread),]

head(upsetsbySpread)
cfblogos <- read.csv("/Users/walkerbasham/Desktop/FBALLstatsWithR/CFBAnalysis/logoLinks.csv") %>% select(school, logo, color,conference, alt_color)
upsets$homelogo<-"1"
upsetsbySpread$homelogo<-"1"
upsets$awayLogo<-"1"
upsetsbySpread$awayLogo<-"1"
head(upsetsbySpread)
head(cfblogos)

upsetsbySpread$homelogo[cfblogos$school==upsetsbySpread$homeTeam] <- cfblogos$logo
upsetsbySpread$homelogo <- cfblogos$logo[match(upsetsbySpread$homeTeam, cfblogos$school)]
head(upsetsbySpread)
upsetsbySpread$awayLogo <- cfblogos$logo[match(upsetsbySpread$awayTeam, cfblogos$school)]
upsetsbySpread$excitement<-0
excitement<-cfb_game_info(2019)

upsetsbySpread$excitement <- excitement$excitement_index[match(upsetsbySpread$id, excitement$id)]
str(upsetsbySpread)
upsetsbySpread$excitement <- as.numeric(upsetsbySpread$excitement)
upsetsbySpread$excitement<-round(upsetsbySpread$excitement, digits = 2)

tableData<- upsetsbySpread%>% select(season, week,homeTeam, homelogo, awayTeam, awayLogo,formattedSpread,homeScore,awayScore,excitement)
tableData<-tableData[1:20,]
head(tableData)

tableData %>%
  head() %>%
  gt()
tableData<-as_tibble(tableData)
tableByspreadValue<-tableData %>%
  gt() %>%
  text_transform(
    locations = cells_body(vars(homelogo)),
    fn = function(x) {
      web_image(
        url = x,
        height = as.numeric(25)
      )
    }
  )%>%
    text_transform(
      locations = cells_body(vars(awayLogo)),
      fn = function(x) {
        web_image(
          url = x,
          height = as.numeric(25)
        )
      }
  )%>%
  data_color(
    columns = vars(excitement),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
 
  tab_source_note("TABLE: @super_bash_br0s | DATA: @cfb_data") %>%
  tab_header(
    title = md("Biggest Upsets in FBS football"),
    subtitle = "2019 Season (spread is 3 points or more)"
  )

gtsave(tableByspreadValue, "/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/upsetTablebySpread2.png")


upsetsbyExcitement<-upsetsbySpread[order(-upsetsbySpread$excitement),]

tableData<- upsetsbyExcitement%>% select(season, week,homeTeam, homelogo, awayTeam, awayLogo,formattedSpread,homeScore,awayScore,excitement)
tableData<-tableData[1:20,]
head(tableData)

tableData %>%
  head() %>%
  gt()
tableData<-as_tibble(tableData)
playersTable<-tableData %>%
  gt() %>%
  text_transform(
    locations = cells_body(vars(homelogo)),
    fn = function(x) {
      web_image(
        url = x,
        height = as.numeric(25)
      )
    }
  )%>%
  text_transform(
    locations = cells_body(vars(awayLogo)),
    fn = function(x) {
      web_image(
        url = x,
        height = as.numeric(25)
      )
    }
  )%>%
  data_color(
    columns = vars(excitement),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
  
  tab_source_note("TABLE: @super_bash_br0s | DATA: @cfb_data") %>%
  tab_header(
    title = md("Most Exciting Upsets in FBS football"),
    subtitle = "2019 Season (spread is 3 points or more)"
  )

gtsave(playersTable, "/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/upsetsByExcitementTable2.png")



