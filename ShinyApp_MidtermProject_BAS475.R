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
library(feasts)
library(ggeasy)
library(ggthemes)
library(seasonal)
library(seasonalview)

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
                 dashboardHeader(title = "Interest in \"Tennessee Lady Vols\"", titleWidth = 500),
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
                                      \"Tennessee Lady Vols\" from data collected by 
                                      GoogleTrends."),
                               
                               tags$head(tags$style('h3 {color:#FF8200;}')),
                               
                               tags$br(),
                               
                               tags$h3("The second tab displays the Full-Time Series graphic for the interest in \"Tennessee Lady Vols\" from January 2004 to March 2022."),
                             
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
                             ),
                             
                             hr(),
                             
                             h3("Interpretation"),
                             
                             h4("The full-time series shows a trend that was 
                                relatively increasing from 2004-2008.The trend
                                then appears to be decreasing from about
                                2009-2017. The trend then appears to increase 
                                from 2018-2022. There appears to be strong
                                seasonality throughtout the plot. This is 
                                likely due to basketball season.")
                             
                     ), 
                     
                     # Third tab content
                     tabItem(tabName = "graph2",
                             h1("Graphic of Your Choice"), 
                             
                             hr(),
                             
                             radioButtons("plot_type", 
                                          label = h2("Which plot do you want to see?"),
                                          choices = c("Seasonality", 
                                                      "Autocorrelation", 
                                                      "Decomposition")),
                             
                             hr(),
                             
                             plotOutput("myplot"),
                             
                             hr(),
                             
                             h3("Interpretation"),
                             
                             textOutput("myplotint")
                        
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
      labs(title = "The Interest of \"Tennessee Lady Vols\"", y = "Interest") +
      ggeasy::easy_center_title() +
      ggeasy::easy_all_text_color(color = "#ff8200") +
      theme(plot.background = element_rect(fill = "white"), 
            panel.background = element_rect(fill = "white"))
    ggplotly(p)
  })
  
  output$myplot <- renderPlot({
    if (input$plot_type == "Seasonality") {
      g_trends %>% gg_season(Interest)+
        theme_fivethirtyeight()+
        labs(title = "The Interest of \"Tennessee Lady Vols\"", y = "Interest") +
        ggeasy::easy_center_title() +
        ggeasy::easy_all_text_color(color = "#ff8200") +
        theme(plot.background = element_rect(fill = "white"), 
              panel.background = element_rect(fill = "white"))
    } 
    else if (input$plot_type == "Autocorrelation") {
      g_trends %>% ACF() %>% 
        autoplot()+
        labs(title = "Interest of Tennessee Lady Vols")+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_colour(colour = "#FF8200")+
        theme(plot.background = element_rect(fill = "white"), 
              panel.background = element_rect(fill = "white"))
    }
    else if (input$plot_type == "Decomposition") {
      x11_dcmp <- g_trends %>%
        model(x11 = X_13ARIMA_SEATS(Interest ~ x11())) %>%
        components()
      autoplot(x11_dcmp) +
        labs(title = "Decomposition of Interest of \"Tennessee Lady 
           Vols\" using X-11.")+
        ggeasy::easy_center_title()+
        ggeasy::easy_all_text_colour(colour = "#FF8200")+
        theme(plot.background = element_rect(fill = "white"), 
              panel.background = element_rect(fill = "white"))
    }
  })
  
  output$myplotint <- renderText({
    if (input$plot_type == "Seasonality") {
      noquote(paste(c("The seasonality plot shows that the interest 
      in \"Tennessee Lady Vols\" peaks from January until April and 
      again from November to December. This makes logical sense 
      because this coincides with basketball season. Also of note is 
      that June is particularly high for most of the summer months.
      This is likely due to the conclusion of softball season. The
      highest amount of interest seems to coincide with Tennessee's
      last two women's basketball national championships in 2007 and 
      2008.", 
      collapse = " ")))
    } 
    else if (input$plot_type == "Autocorrelation") {
      noquote(paste(c("The autocorrelation plot shows that the 
      interest in \"Tennessee Lady Vols\" is extremely seasonal. This
      is likely due to basketball season and softball season. 
      This is especially the case for seasons that these two teams
      experience success.", collapse = " ")))
    }
    else if (input$plot_type == "Decomposition") {
      noquote(paste(c("The X11 decomposition plot shows that the trend
      peaked in about 2008. The plot also shows a consistent amount
      of seasonality.", collapse = " ")))
    }
  })
  
  
  
}
shinyApp(ui, server)



