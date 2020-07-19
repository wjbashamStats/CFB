library(jsonlite)
library(dplyr)
library(tidyverse)
library(ggimage)
library(cfbscrapR)
library(grid)
library(png)
library(tibble)

cfbWRusage <- function(year){
  
  # by position groups
  base_url = "https://api.collegefootballdata.com/player/usage?year="
  full_url = paste0(base_url,year,"&conference=SEC&position=WR&excludeGarbageTime=true", flatten = TRUE)
  print(full_url)
  df <- fromJSON(full_url, flatten = TRUE)
  return(df)
  
  
  # base_url = paste0("https://api.collegefootballdata.com/recruiting/players?year=",year)
  # if(!is.null(team)){
  #   team = URLencode(team, reserved = T)
  #   full_url = paste0(base_url,"&team=",team)
  #   df = fromJSON(full_url)
  #   return(df)
  # }
  # 
  # df = fromJSON(base_url)
  # return(df)
}

usage <- cfbWRusage(2019)
head(usage)
cfblogos <- read.csv("/Users/walkerbasham/Desktop/FBALLstatsWithR/CFBAnalysis/logoLinks.csv") %>% select(school, logo, color,conference, alt_color)
#print(cfblogos)

#join the logos and the conferences to the third down summary data using offense play and school as a key
chartdata1 <- usage %>% left_join(cfblogos, by = c("team" = "school"))

print(chartdata1)

files <- list.files(path="/Users/walkerbasham/Desktop/FBALLstatsWithR/CFBAnalysis/logos", pattern="*.png", full.names=TRUE, recursive=FALSE)

for (i in 1:nrow(chartdata1)) {
  school_name <- chartdata1[i,"team"] #store whatever row we're on's value in offense_play column in school name
  school_name_finder <- paste(school_name,".png") #add .png to the name of the school for the file lookup
  for (j in files) {
    file_name_finder <- (substring(j, 63)) #get rid of the first 63 characters of the file name we are currently on
    if (school_name_finder == file_name_finder) { #compare them together
      chartdata1[i,"logoPath"] = j #if they match, add the path to the logo file in our chartdata
      #print(newimage)
    }
  }
}
m <- png::readPNG("/Users/walkerbasham/Desktop/FBALLstatsWithR/CFBAnalysis/logos/SEC/SECLOGO.png") #select the sec image and load as png
w <- matrix(rgb(m[,,1],m[,,2],m[,,3], m[,,4] * 0.2), nrow=dim(m)[1]) #0.2 is alpha, adjust the opacity of m

chartdata1<-chartdata1[2:21,] #2-21 because lynn bowden was classified as WR
print(class(chartdata1$alt_color))

chartdata1 <- chartdata1[order(chartdata1$usage.overall),]

chartdata1<-as_tibble(chartdata1)
print(chartdata1)

PLOT = ggplot(chartdata1) + #data with ggplot
  #scale_fill_continuous(guide = FALSE) + #need this for background
  annotation_custom(rasterGrob(w, width = unit(1,"npc"), height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +  #all this was also for background image formatting
  geom_histogram(aes(x=reorder(name,usage.overall), y=usage.overall), stat="identity", fill=chartdata1$color, color =chartdata1$alt_color) +
  geom_line(aes(x=reorder(name,usage.overall), y = usage.thirdDown, group = 1, colour="Usage Rate on 3rd Down"),  alpha = 1) +
  geom_point(aes(x=reorder(name,usage.overall), y = usage.thirdDown, group = 1, colour="Usage Rate on 3rd Down"),  size = 4,alpha = 1) +
  scale_colour_manual(values = "green", label = "Usage Rate on 3rd Down")+
  
  
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,.3),
                     breaks = seq(0,.3,.03)) + #limit on graph height and start directly at 0 #limit on graph height
  
  labs(y = "Overall Usage Rate",
       colour = "",
       x = "Player",
       caption = "Figure: @super_bash_br0s | Data: @CFB_data with #cfbscrapR",
       title = "Overall Usage Rate",
       subtitle = "SEC Wide Receivers 2019 Season (Garbage Time Removed)") +
  #Labs adds text
  theme_grey() +
  theme( #set element text sizes and other formatting stuff like legends
    legend.justification = "center",
    legend.position = ("bottom"),
    legend.title = element_text(colour="Black", size=10, 
                                face="bold"),
    legend.text  = element_text(colour="Black", size=10, 
                                face="bold"),
    legend.background = element_rect(fill="white", 
                                     size=0.01, linetype="solid"),
    legend.key = element_blank(),  # Key background    
    
    axis.text.x = element_text(size = 9,angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "bold"),
    plot.caption = element_text(size = 12, face = "bold"))


#add images. first create a list  
g = list()
for(i in 1:nrow(chartdata1)){ #iterate through dataframe
  img = readPNG(chartdata1$logoPath[i]) #(load png)
  g[[i]] =  rasterGrob(img, interpolate=TRUE) #rasterize the image
  
  PLOT = PLOT + #now add this to plot, plus add the image below and lines with plot
    annotation_custom(grob=g[[i]], xmin=i-.5, xmax=i+.5, ymin=chartdata1$usage.overall[i], ymax=chartdata1$usage.overall[i]+.05) 
  #^adds each image. xmax and xmin - horizontal position. ymin and ymax - vertical position. 
}

#finish plotting
PLOT
ggsave(filename = file.path("/Users/walkerbasham/Desktop/FBALLstatsWithR/CFBAnalysis/UsageRate","SECWRusagerate.png"))
#print(chartdata1)

