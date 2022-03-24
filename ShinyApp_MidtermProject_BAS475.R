library(shiny)
library(fpp3)
library(ggplot2)
library(shinyWidgets)
library(quantmod)
library(plotly)
library(dplyr)
library(shinydashboard)
library(tidyquant)
library(flexdashboard)
library(ggeasy)
library(ggthemes)

# Path where data is
file_path <- "multiTimeline.csv"
# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv(file_path, skip = 2)
# Rename columns
names(g_trends) <- c("Month", "Interest")
# Convert Month to date
g_trends$Month <- yearmonth(g_trends$Month)
# Convert to tsibble
g_trends <- tsibble(g_trends)


ui <-
  dashboardPage( skin = "yellow",
                 dashboardHeader(title = "Title", titleWidth = 200),
                 dashboardSidebar( width = 200,
                                   sidebarMenu(
                                     menuItem("Introduction", tabName = "intro", 
                                              icon = icon("dashboard")),
                                     menuItem("Full-Time Series", tabName = "graph1", 
                                              icon = icon("chart-line")),
                                     menuItem("Plot Choice", tabName = "graph2", 
                                              icon = icon("chart-line")),
                                     menuItem("My Feature", tabName = "feature", 
                                              icon = icon("chart-line"))
                                   )
                 ),
                 dashboardBody(
                   tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
                   
                   tabItems(
                     # First tab content
                     tabItem(tabName = "intro",
                             h1("Introduction"), 
                             
                             hr(),
                             
                             tags$div(
                               tags$h3("This application analyzes the interest in 
                                      'Tennessee Lady Vols' from data collected by 
                                      GoogleTrends."),
                               
                               tags$head(tags$style('h3 {color:#FF8200;}')),
                               
                               tags$br(),
                               
                               tags$h3("The second tab displays the Full-Time Series graphic for the interest in 'Tennessee Lady Vols' from January 2004 to March 2022."),
                             
                               tags$br(),
                               
                               tags$h3("The third tab displays your choice in one of three types of graphics: (1) seasonality, (2) autocorrelation, and (3) decomposition. "),
                               
                               tags$br(),
                               
                               tags$h3("The fourth tab displays something."),
                               
                               tags$br(),
                               
                               ),
                             div(img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/fc/Tennessee_Lady_Volunteers_logo.svg/1200px-Tennessee_Lady_Volunteers_logo.svg.png",
                                     height = 200, width = 200),
                                 style="text-align: center;")
                
                     ),
                     
                     # Second tab content
                     tabItem(tabName = "graph1",
                             h1("Full-Time Series Graph"),
                             
                             hr(),
                             
                             basicPage(
                               plotlyOutput("fulltimeseries")
                             )
                     ), 
                     
                     # Third tab content
                     tabItem(tabName = "graph2",
                             h1("Graphic of Your Choice") 
                     ),
                     
                     # Fourth tab content
                     tabItem(tabName = "feature",
                             h2("My Feature")
                             
                             
                             
                     )
                     
                   )  
                   
                 )

       )



server <- function(input, output, session) {
  output$fulltimeseries <- renderPlotly({
    p <- ggplot(g_trends, aes(Month, Interest)) + 
      geom_line(color = "#22afff") + 
      theme_fivethirtyeight()+
      labs(title = "The Interest of 'Tennessee Lady Vols'", y = "Interest") +
      ggeasy::easy_center_title() +
      ggeasy::easy_all_text_color(color = "#ff8200") +
      theme(plot.background = element_rect(fill = "white"), 
            panel.background = element_rect(fill = "white"))
    ggplotly(p)
  })
}
shinyApp(ui, server)



