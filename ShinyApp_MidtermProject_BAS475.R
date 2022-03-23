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
                             h1("Introduction")
                
                     ),
                     
                     # Second tab content
                     tabItem(tabName = "graph1",
                             h1("Full-Time Series Graph"),
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



