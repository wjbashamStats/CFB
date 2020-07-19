library(jsonlite)
library(dplyr)
library(tidyverse)
library(ggimage)
library(cfbscrapR)
library(gridExtra)
library(png)
library(tibble)
library(webshot)
library(stats)
library(tidyr)
library(cluster)
library(stats)
library(factoextra)
library(ggrepel)
library(tidymodels)
library(ggrepel)
library(gt)
library(paletteer) # for all the palettes
library(ggplot2)


makeGraphPPADensity<-function(data,yr){
  
  
  data<- filter(data,Year==yr)
  print(data$alt_color)
  data$category <- as.factor(data$category)
  
  
  p1=data %>% ggplot(aes(x=offense.cumulative.total,y=totalPPA.all)) + 
    #geom_label_repel(aes(label = name), color = "white",fill = as.factor(data$color), size = 3, segment.color = "black", hjust = .5, force = 3, max.iter = 9999) + 
    geom_point(aes(colour = as.factor(category)), alpha = .001,)+
    geom_image(image = data$logo, size = .025) +
    geom_image(aes(image = logo, color = as.factor(category)),alpha = .5, size = .025 ) +
    
    geom_hline(aes(yintercept = mean(totalPPA.all), colour = "purple"), linetype = "dashed") +
    geom_vline(aes(xintercept = mean(offense.cumulative.total), colour = "firebrick"),linetype = "dashed") +
    #geom_smooth(method="lm", se=F) + 
    scale_colour_manual(values = c("green", "gold","hotpink","purple","firebrick"), label = c("Below Average", "Average","Above Average","Avg Total QB PPA", "Avg Offensive Cumulative EPA")) +
    scale_x_continuous(expand = c(0,0),
                       limits = c(-100,500),
                       breaks = seq(-100,500,25)) + #limit on graph height and start directly at 0 #limit on graph height
    scale_y_continuous(expand = c(0,0),
                       limits = c(-100,500),
                       breaks = seq(-100,500,25)) + #limit on graph width and start directly at -100 #limit on graph height
    
    labs(y = "Total QB PPA",
         x = "Offensive Cumulative PPA",
         caption = "Figure: @super_bash_br0s | Data: @CFB_data with #cfbscrapR",
         title = "QB and Offensive Production",
         subtitle = paste0(yr," Season (min 250 snaps)")) +
    theme_gray() +
    theme(
      legend.justification = "center",
      legend.position = ("bottom"),
      legend.title = element_blank(),
      legend.text  = element_text(colour="Black", size=18, 
                                  face="bold"),
      legend.background = element_rect(fill="white", 
                                       size=0.01, linetype="solid"),
      legend.key = element_blank(),  # Key background    
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 18, face = "bold"),
      plot.caption = element_text(size = 14, face = "bold"))
  
  p2<-ggMarginal(p1, type="density", groupColour = TRUE, groupFill = TRUE)
  ggsave(filename = file.path(paste0('/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/PPA/Position/QB/PPAbySeasonQBTeam', yr, '.png')), plot = p2, height = 20 , width = 20)
  
  # ggsave(filename = file.path('/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/PPA/Position/QB/PPAbySeasonQBGraph2018byTeam3centersstep2.png'))
  # ggplot(data, aes(x = totalPPA.all)) +
  #   stat_function(
  #     fun = dnorm,
  #     args = with(data, c(data = mean(data$totalPPA.all), sd = sd(data$totalPPA.all)))
  #   ) +
  #   scale_x_continuous("Total PP1A")
  # ggsave(paste0("/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/PPA/Position/QB/PPAbySeasonQBGraph", yr, ".png"), height = 10 , width = 20)
}


makeTable<-function(data, yr){
  data<- filter(data,Year==yr)
  tableData<- data%>% select(Year, name,logo, category, totalPPA.all, offense.cumulative.total)
  tableData$category <- as.character(tableData$category)
  tableData$category[tableData$category == "1"] <- "Above Average"
  tableData$category[tableData$category == "2"] <- "Average"
  tableData$category[tableData$category == "3"] <- "Below Average"
  
  colnames(tableData)[1] = "Season"
  colnames(tableData)[2] = "Player"
  colnames(tableData)[4] = "Player/Team Production"
  
  colnames(tableData)[5] = "Player_Total_PPA"
  colnames(tableData)[3] = "School"
  colnames(tableData)[6] = "Offense_Total_PPA"
  
  
  print(tableData)
  
  
  

    playersTable<-tableData %>%
      head() %>%
      gt()
    tableData <- tableData[order(tableData$Player_Total_PPA, decreasing = TRUE),]  
    tableData<-as_tibble(tableData)
    playersTable <- tableData %>%
      gt() %>%
      text_transform(
        locations = cells_body(vars(School)),
        fn = function(x) {
          web_image(
            url = x,
            height = as.numeric(25)
          )
        }
      )%>%
      data_color(
        columns = vars(Player_Total_PPA),
        colors = scales::col_numeric(
          # custom defined values - notice that order matters!
          palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
          domain = NULL
        )
      ) %>%
      data_color(
        columns = vars(Offense_Total_PPA),
        colors = scales::col_numeric(
          # custom defined values - notice that order matters!
          palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
          domain = NULL
        )
      )%>%
      
      tab_source_note("TABLE: @super_bash_br0s | DATA: @CFB_DATA with #cfbscrapr") %>%
      tab_header(
        title = md("QB and Team PPA"),
        subtitle = paste0(yr, " season (min 250 snaps)")
      )%>%
      tab_style(
        style = list(
          cell_borders(
            sides = "left",
            color = "black",
            weight = px(3)
          )
        ),
        locations = list(
          cells_body(
            columns = vars(Player_Total_PPA)
          )
        )
      ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "bottom",
          color = "black",
          weight = px(3)
        )
      ),
      locations = list(
        cells_column_labels(
          columns = gt::everything()
        )
      )
      
    )
    
    
    gtsave(playersTable, paste0("/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/PPA/Position/QB/RankingsByQBTable", yr, ".png"))
}


QBInfo <- read.csv(file = "/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/PPA/Position/QB/PPAbySeasonQB.csv")
coverInfo<-read.csv(file = "/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/Gambling/CoverPctByTeambySeason.csv")
print(QBInfo)

QBInfo <- coverInfo %>% left_join(QBInfo, by = c("Year" = "season", "School" ="team"))
print(QBInfo)


cfblogos <- read.csv("/Users/walkerbasham/Desktop/FBALLstatsWithR/CFBAnalysis/logoLinks.csv") %>% select(school, logo, color,conference, alt_color)
chartdata1 <- QBInfo %>% left_join(cfblogos, by = c("School" = "school"))
print(chartdata1)


ppa2019 = fromJSON("https://api.collegefootballdata.com/ppa/teams?year=2019", flatten = TRUE)
ppa2018 = fromJSON("https://api.collegefootballdata.com/ppa/teams?year=2018", flatten = TRUE)
ppa2017 = fromJSON("https://api.collegefootballdata.com/ppa/teams?year=2017", flatten = TRUE)
ppa2016 = fromJSON("https://api.collegefootballdata.com/ppa/teams?year=2016", flatten = TRUE)
ppa2015 = fromJSON("https://api.collegefootballdata.com/ppa/teams?year=2015", flatten = TRUE)
print(ppa2019)
print(ppa2019)

ppaAllTeamsAllYears<-bind_rows(ppa2019,ppa2018,ppa2017,ppa2016,ppa2015)

ppaAllTeamsAllYearsWithPlayers <- chartdata1 %>% left_join(ppaAllTeamsAllYears, by = c("Year" = "season", "School" ="team"))
ppaAllTeamsAllYearsWithPlayers <- na.omit(ppaAllTeamsAllYearsWithPlayers)
data<-ppaAllTeamsAllYearsWithPlayers %>% select(Year, name, CovPct, totalPPA.all, logo, color, alt_color,offense.overall,offense.passing,offense.cumulative.total,offense.cumulative.passing)
print(data)
data$offense.cumulative.total<-as.numeric(data$offense.cumulative.total)
data$totalPPA.all<-as.numeric(data$totalPPA.all)
print(typeof(data$offense.cumulative.total))
qbsubset <- scale(data[ , c(4,10)])

clusterAnalysis2019_model <- kmeans(qbsubset, centers = 3,
                                    iter.max = 10,
                                    nstart = 25)


data$category <- as.factor(clusterAnalysis2019_model$cluster)
print(data)
str(data)






centroids <- data.frame(cluster = factor(seq(1:3)),
                        totalPPA.all = clusterAnalysis2019_model$centers[,'totalPPA.all'],
                        CovPct = clusterAnalysis2019_model$centers[,'offense.cumulative.total'])

######## DATA PREP FOR PPA QBS is done ################
for(x in 2019:2015){
  plot <- makeGraphPPADensity(data,x)
  #plot1<-makeTable(data,x)
}


