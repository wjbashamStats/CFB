library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(ggimage)
library(dplyr)  # For filtering and manipulating data
library(png)
library(grid)
library(stringr)

dataset1<-read.csv("./Data/ProjData3.csv")

TextGenerator <- function(OorD,statistic,downs){
    if (OorD == "Defense") {
        if (str_detect(statistic,"YPP")) {
            title<-paste0("Defensive Allowed ", statistic," on ", downs, " down")
            title <-gsub("\\.", " ",title)
            return(title)
        }
        else{
            title<-paste0("Defensive ", statistic," on ", downs, " down")
            title <-gsub("\\.", " ",title)
            return(title)
        }
    }
    else{
        title<-paste0("Offensive ", statistic," on ", downs, " down")
        title <-gsub("\\.", " ",title)
        return(title)
    }
    return(paste0(OorD,statistic,downs))
 }

ui <- fluidPage(
    titlePanel(
        div(
            tags$em(style = "font-size: 30px; display: block; font-style: normal",
                    "CFB Down Examination by Team",
                    br()
            ),
        )
    ),
    title = "CFB Down Examination",
    plotOutput("plot", hover = "plot_hover", height = "auto"), #need height to be auto for the function below to work
    hr(),
    #Creates the sidebar for the app
    fluidRow(  
        column(4,
        #allows the user to filter by school or conference
            selectInput(inputId = "Filterchoice",        
                label = "Filter by Team or Conferences?",
                choices = list("Team", "Conference"),
                selected = "Conference"),
            selectInput(inputId = "OffenseDefense",        
                label = "Offense or Defense?",
                choices = unique(dataset1$OffenseOrDefense)
            ),
            #create
            ##Conditional Input Section!!!
            #if the first selectinput is conference, display this one
            conditionalPanel(condition = "input.Filterchoice == 'Conference'",
                selectInput(inputId = "Conference", 
                    label = "Conferences", 
                    choices = unique(dataset1$conference), 
                    selected = "SEC",
                    multiple = TRUE)
            ),
            #if the first selectinput is school, display this one
            conditionalPanel(condition = "input.Filterchoice == 'Team'",
                selectInput(inputId = "School", 
                    label = "Teams", 
                    choices = unique(dataset1$team), 
                    selected = c("Alabama","Notre Dame", "Ohio State", "Clemson"),
                    multiple = TRUE)
            ),
        ),
        column(4,
            #create Statistic Input for user
            selectInput(inputId = "Stat",
                label = "Select Stat for x axis",
                choices = names(dataset1)[4:17],
                selected = "epa.overall"),
            #Create Down Input for user
            selectInput(inputId = "Down",
                label = "Select Down to examine",
                choices = unique(dataset1$down),
                selected = "1"),
        ),
        
        column(3,
            selectInput(inputId = "Images",
                        label = "Logos On Plot?",
                        choices = c("Yes", "No"),
                        selected = "Yes"),
            selectInput(inputId = "AverageOnGraph",
                        label = "Average On Plot?",
                        choices = c("Yes", "No"),
                        selected = "Yes"),
            sliderInput("plotHt", "Choose Plot Height (in pixels)", value = 500, min = 10, max = 2000),
            downloadButton("downloadData","Download"),
        
        )
    )
)

server <- function(input, output, session) {
    
    #Filter the dataset based on the input from the school or conference filter!
    offenseOrDefense<-reactive({
        filter(dataset1,OffenseOrDefense %in% input$OffenseDefense)
    })
    
    choice <-reactive({
        if (input$Filterchoice == 'Conference') {
            filter(offenseOrDefense(),conference %in% input$Conference)
        }
        else{
            filter(offenseOrDefense(),team %in% input$School)
        }
    })
    
    #now filter from downs, based on the choice of input (choice is the function that creates teh dataset that downs is filtering on)
    down <- reactive({
        req(input$Down)
        filter(choice(), down == input$Down)
    })
    #get all logo paths

    
    OffenseDefensePaste <- reactive(paste0(input$OffenseDefense))
    #get the string version of the statistic chosen for plot aesthetics
    statPaste <- reactive(paste0(input$Stat))
    #get the string version of the down chosen for plot aesthetics
    downPaste<-reactive(paste0(input$Down))
    LogosYesNo<-reactive(paste0(input$Images))
    AverageYesNo<-reactive(paste0(input$AverageOnGraph))
    
    
    #get the plot height inputted by user
    ht<-reactive(input$plotHt)
    
    #create plot
    output$plot <- renderPlot({
        ggplot(down()) + #use the down dataset created in the down reactive unction 
        geom_histogram(aes(x=reorder(team, get(input$Stat)), y=get(input$Stat)), stat = "identity", fill = down()$color, color = down()$alt_color) +
        {if(LogosYesNo() == "Yes")
            ggimage::geom_image(aes(x = reorder(team, get(input$Stat)), y = get(input$Stat),image = logo),size = 1/length(down()[,statPaste()]), by = 'height', asp = 16/9)
        }+
        {if(AverageYesNo() == "Yes")
            geom_hline(aes(yintercept=mean(get(input$Stat))), color="green", linetype="dashed", size=1)
        }+
 
        labs(y = TextGenerator(OffenseDefensePaste(), statPaste(), downPaste()),
              colour = "",
              x = "School",
              caption = "Figure: @super_bash_br0s with Shiny | Data: @CFB_data with #cfbfastr",
              title = TextGenerator(OffenseDefensePaste(), statPaste(), downPaste()),
              subtitle = "2020 Season")+
        coord_flip() + #flip coordinates, much easier to read
             #Labs adds text
         theme_gray() +
         theme( #set element text sizes and other formatting stuff like legends
             axis.text.x = element_text(size = 10, face = "bold"),
             axis.ticks.y =  element_blank(),
             axis.title.x = element_text(size = 12, face = "bold"),
             axis.title.y = element_text(size = 12, face = "bold"),
             axis.text.y = element_text(size = 10, face = "bold", color = "black"),
             plot.title = element_text(size = 16, face = "bold"),
             plot.subtitle = element_text(size = 14, face = "bold"),
             plot.caption = element_text(size = 10, face = "bold"))

            #adds a line for visibility
        
        },
        #function that allows the plot height to change dynamically, this is part of the renderplot function()
        height = function() {input$plotHt}
    )
    output$downloadData <- downloadHandler(
        filename = function() {
            "SchoolDownsData.csv"
        }, 
        content = function(file) {
            write.csv(dataset1, file, row.names = FALSE)
        }
    )
}

shinyApp(ui, server)


