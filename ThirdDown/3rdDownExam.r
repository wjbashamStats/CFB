library(dplyr)
library(tidyverse)
library(ggimage)
library(cfbscrapR)
library(grid)
library(png)
library(tibble)
library(scales)
pbp_data<-data.frame()
#import PBP Data
pbp_data<-cfb_pbp_data(
  year = 2020,
  week = 1,
  epa_wpa = TRUE
)
print(pbp_data)

for (x in 2:7) {
  pbp_weekly_data<-cfb_pbp_data(
    year = 2020,
    week = x,
    epa_wpa = TRUE
  )
  pbp_data<-rbind(pbp_data,pbp_weekly_data)
}

pbp_data<-as_tibble(pbp_data)
pbp_data
#Import prepared Gambling Data
#gambling_info<-read.csv(file = "/Users/walkerbasham/Desktop/FBALLstatsWithR/2020Season/CoverAnalysis/Week2/2020week2Coverinfo.csv")
#gambling_info <- gambling_info %
#gambling_info <- gambling_info %>% select (game_id, totalHit, spreadWinner)


ThirdDownPlays19 <- pbp_data %>% filter(down == 3) #Filter for third down plays only

ThirdDownSummary <- ThirdDownPlays19 %>% filter((rush == 1 | pass == 1) & penalty_flag == FALSE) %>% #all rushes and passes
  group_by(offense_play) %>% #group by school on offense
  summarise(
            epa_play = round(mean(EPA),2),
            epa_rush = round(mean(EPA[rush==1]),2), #calc the mean of the yards gained per rush on third down
            epa_pass = round(mean(EPA[pass==1]),2), #calc the mean of the yards gained per pass on third down
            avg_ytg_run = mean(distance[rush==1]), #calc the mean of yards to go on third when they run
            avg_ytg_pass = mean(distance[pass==1]), #calc the mean of yards to go on third when they run
            avg_ytg = mean(distance),
            num_plays = n(),
            num_games = n_distinct(game_id)) #Filter out FCS Schools
str(ThirdDownSummary)

cfblogos <- read.csv("/Users/walkerbasham/Desktop/FBALLstatsWithR/CFBAnalysis/logoLinks.csv") %>% select(school, logo, color,conference, alt_color)


chartdata1 <- ThirdDownSummary %>% left_join(cfblogos, by = c("offense_play" = "school"))


#chartdata1<-chartdata1%>% left_join(gambling_info, by = c("game_id" = "game_id"))
#chartdata1$cover = "NA"

# for (i in 1:length(chartdata1$cover)) {
#   if (chartdata1$spreadWinner[i] == chartdata1$offense_play[i]) {
#     chartdata1$cover[i] = "Yes"
#   }
#   else if(chartdata1$spreadWinner[i] != chartdata1$offense_play[i]) {
#     chartdata1$cover[i] = "No"
#   }
#   else{
#     chartdata1$cover[i] = "Push"
#   }
#   
# }



chartdata1 %>% ggplot(aes(x=avg_ytg_pass, y=avg_ytg_run)) + geom_image(image = chartdata1$logo, asp = 16/9) +
  
  #geom_image(aes(image = logo, colour = as.factor(cover)), alpha = .75, asp = 16/9) +
  geom_vline(xintercept = mean(chartdata1$avg_ytg_pass), linetype = "dashed", color = "green") +
  geom_hline(yintercept = mean(chartdata1$avg_ytg_run), linetype = "dashed", color = "green") +
  geom_smooth(method ="lm", se = FALSE) +
  labs(y = "Avg YTG when 3rd Down Playcall is a Run",
       x = "Avg YTG when 3rd Down Playcall is a Pass",
       caption = "Figure: @super_bash_br0s | Data: @CFB_data with #cfbscrapR",
       title = "3rd Down Play Call YTG Examination",
       subtitle = "2020 Season Through Week 7") +
  theme_bw() +
  theme(
    legend.position = ("bottom"),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 10))


ggsave('/Users/walkerbasham/Desktop/FBALLstatsWithR/2020Season/ThirdDown/Week7/ThirdownYTGcomparison.png')


chartdata1 %>% ggplot(aes(x=avg_ytg, y=epa_play)) + geom_image(image = chartdata1$logo, asp = 16/9) +
  
  #geom_image(aes(image = logo, colour = as.factor(cover)), alpha = .75, asp = 16/9) +
  geom_vline(xintercept = mean(chartdata1$avg_ytg), linetype = "dashed", color = "green") +
  geom_hline(yintercept = mean(chartdata1$epa_play), linetype = "dashed", color = "green") +
  geom_smooth(method ="lm", se = FALSE)+
  labs(y = "EPA/3rdDown",
       x = "Avg YTG on 3rd Down",
       caption = "Figure: @super_bash_br0s | Data: @CFB_data with #cfbscrapR",
       title = "3rd Down EPA/YTG",
       subtitle = "2020 Season Through Week 7") +
  theme_bw() +
  theme(
    legend.position = ("bottom"),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 10))


ggsave('/Users/walkerbasham/Desktop/FBALLstatsWithR/2020Season/ThirdDown/Week7/ThirdownYTGPPA.png')


chartdata1 %>% ggplot(aes(x=avg_ytg, y=num_plays/num_games)) + geom_image(image = chartdata1$logo, asp = 16/9) +
  
  #geom_image(aes(image = logo, colour = as.factor(cover)), alpha = .75, asp = 16/9) +
  geom_vline(xintercept = mean(chartdata1$avg_ytg), linetype = "dashed", color = "green") +
  geom_hline(yintercept = mean(chartdata1$num_plays/chartdata1$num_games), linetype = "dashed", color = "green") +
  geom_smooth(method ="lm", se = FALSE) +
  labs(y = "3rd Downs Per Game",
       x = "Avg YTG on 3rd Down",
       caption = "Figure: @super_bash_br0s | Data: @CFB_data with #cfbscrapR",
       title = "3rd Down",
       subtitle = "2020 Season Through Week 7") +
  scale_y_continuous(
    breaks = pretty_breaks()
  ) +
  theme_bw() +
  theme(
    legend.position = ("bottom"),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 10))


ggsave('/Users/walkerbasham/Desktop/FBALLstatsWithR/2020Season/ThirdDown/Week7/Thirdown.png')
