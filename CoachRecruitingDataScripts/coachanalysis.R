library(gt) # for static tables
library(tidyverse) # all the things
library(scales)
library(dplyr)
library(webshot)
library(paletteer)
library(cfbscrapR)
webshot::install_phantomjs()
#library(paletteer) # for all the palettes

coaches <- read_csv("/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/Recruiting/RankingsByYearByCoach2020Active.csv")

glimpse(coaches)

coaches <- coaches[order(coaches$`2020`, decreasing = TRUE),]  
coaches<-coaches[!(coaches$`2020`==0),]
print(coaches)

coachesTable <- coaches %>%
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
    columns = vars(`2020`),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = vars(`2019`),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  )%>%
data_color(
  columns = vars(`2018`),
  colors = scales::col_numeric(
    # custom defined values - notice that order matters!
    palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
    domain = NULL
  )
) %>%
  data_color(
    columns = vars(`2017`),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  )%>%

  data_color(
    columns = vars(`2016`),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = vars(`2015`),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  )%>%
  data_color(
    columns = vars(`2014`),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = vars(`2013`),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  )  %>%
    data_color(
      columns = vars(`2012`),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    ) %>%
    data_color(
      columns = vars(`2011`),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    )%>%
    data_color(
      columns = vars(`2010`),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    ) %>%
    data_color(
      columns = vars(`2009`),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    ) %>%
  
  data_color(
    columns = vars(`2008`),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
    data_color(
      columns = vars(`2007`),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    )%>%
    data_color(
      columns = vars(`2006`),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    ) %>%
    data_color(
      columns = vars(`2005`),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    )%>%
    data_color(
      columns = vars(`2004`),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    )%>%
  
  data_color(
    columns = vars(`2003`),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
      domain = NULL
    )
  ) %>%
    data_color(
      columns = vars(`2002`),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    )%>%
    data_color(
      columns = vars(`2001`),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    ) %>%
    data_color(
      columns = vars(`2000`),
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
      )
    )%>%
  tab_source_note("TABLE: @super_bash_br0s | DATA: cfbscrapr") %>%
  tab_header(
    title = md("Active FBS Coaches Recruiting Score 2000-2020"),
    subtitle = "As FBS Head Coaches"
  )

gtsave(coachesTable, "/Users/walkerbasham/Desktop/FBALLstatsWithR/Resources/Recruiting/RankingsByCoachTable.pdf")

  
play_stats<-cfb_play_stats_player(401110723)
glimpse(play_stats)
