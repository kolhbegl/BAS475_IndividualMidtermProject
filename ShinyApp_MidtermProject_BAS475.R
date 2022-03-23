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
                             h1("Full-Time Series Graph")
          
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
  
}
shinyApp(ui, server)



