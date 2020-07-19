library(tidyverse)
library(ggimage)
library(cfbscrapR)

chartData1 <- read.csv("/Users/walkerbasham/Desktop/FBALLstatsWithR/ATSvsWL/recordWLATS.csv") #read from the file
chartData2 <- read.csv("/Users/walkerbasham/Desktop/FBALLstatsWithR/ATSvsWL/DONOTEDIT5YEARDATA.csv") #read from the file

print(chartData1)
cfblogos <- read.csv("/Users/walkerbasham/Desktop/FBALLstatsWithR/CFBAnalysis/logoLinks.csv") %>% select(school, logo)
print(cfblogos)
chartData1 <- chartData1 %>% left_join(cfblogos, by = c("Team" = "school"))
print(chartData1)
write.csv(chartData1,"/Users/walkerbasham/Desktop/FBALLstatsWithR/ATSvsWL/logolinker.csv")


chartData1 %>% ggplot(aes(x=Win, y=Cover)) + geom_image(image = chartData1$logo, asp = 16/9) +
  geom_hline(yintercept = .53, linetype = "dashed", color = "red") +
  
  labs(y = "Cover %",
       x = "Win %",
       caption = "Figure: @super_bash_br0s | Data: @teamrankings with #cfbscrapR",
       title = "Win/Loss % vs Cover %",
       subtitle = "2019 Season") +

  theme_bw() +
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 10, face = "bold"))

ggsave(filename = file.path("/Users/walkerbasham/Desktop/FBALLstatsWithR/ATSvsWL/","WLvsCover1.png"))


cfblogos2 <- read.csv("/Users/walkerbasham/Desktop/FBALLstatsWithR/CFBAnalysis/logoLinks.csv") %>% select(school, logo, color,conference, alt_color)
print(cfblogos2)
chartData2 <- chartData2 %>% left_join(cfblogos2, by = c("Team" = "school"))
print(chartData2)
write.csv(chartData2,"/Users/walkerbasham/Desktop/FBALLstatsWithR/ATSvsWL/logolinker5yrs.csv")
print(chartData2)

print(chartData2$logo)

chartData2 %>% ggplot(aes(x=Win, y=Cover)) + geom_image(image = chartData2$logo, asp = 16/9) +
    geom_hline(yintercept = .53, linetype = "dashed", color = "red") +
        
    labs(y = "Cover %",
          x = "Win %",
          caption = "Figure: @super_bash_br0s | Data: @teamrankings with #cfbscrapR",
          title = "Win/Loss % vs Cover % (last 5 years)",
          subtitle = "2015-2019 Season") +
        
    theme_bw() +
    theme(
      axis.text = element_text(size = 10, face = "bold"),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14, face = "bold"),
      plot.caption = element_text(size = 10, face = "bold"))
      
      ggsave(filename = file.path("/Users/walkerbasham/Desktop/FBALLstatsWithR/ATSvsWL/","WLvsCover5yr.png"))
      
      