

########################## SETUP ##############################

library(shiny)
library(here)
library(tidyverse)

daily <- read_csv("daily.csv")
trend_type <- read_csv("trend_type.csv")

########################## UI ##############################

ui <- fluidPage(
   h1("Deep Work Supervisor - Test"),
   # Application title
   titlePanel(""),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("category", "Select categories", choices = c("Work", "Craft", "Study", "Wealth", "Other")),
        dateRangeInput("date_range", "Select date range", min = "2018-01-01", max = "2019-12-31", start = "2018-07-31") 
      ),
      
      # Output
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    
                    tabPanel(title = "Actual",
                             h5("What I have been doing"),
                             plotOutput(outputId = "line_plot"),
                             plotOutput(outputId = "box_plot")),
                    
                    
                    
                    tabPanel(title = "vs 2019 Target",
                             h5("If I am on track, what is the gap?"),
                             verbatimTextOutput(outputId = "summary_stats")),
                    
                    
                    
                    
                    tabPanel(title = "Forecast",
                             h5("What is likely to happen"))

                    
        ) # End tabsetPanel
      ) # End mainPanel
   ) # End sidebarLayout
) # End fluidPage


########################## SERVER ##############################


server <- function(input, output) {
   
  ## Reactive variables
  stats_table <- reactive({
    summary(trend_type %>% 
              filter(category %in% input$category) %>%
              select(time_spent))
    
  })
  
  
  ## ACTUALS 
  
  ### LINE PLOT
   output$line_plot <- renderPlot({
      # generate plot
      x <- trend_type
      # draw the plot
      ggplot(data = x %>%
               filter(category %in% input$category), aes(x = Date, y = time_spent, col = category)) +
        geom_point(alpha = 0.2) +
        geom_smooth() +
        ylim(0, 6) +
        xlim(min(x$Date), max(x$Date)) +
        theme_minimal()
   })
   
   
   ### BOX PLOT
   output$box_plot <- renderPlot({
     # generate plot
     x <- trend_type
     # draw the plot
     ggplot(data = x%>%
              filter(category %in% input$category), aes(x = category, y = time_spent, col = category)) +
       geom_boxplot(alpha = 0.5) +
       ylim(0, 6) +
       theme_minimal()
     
     
   })
   
   
   
   ## TARGET
   output$summary_stats <- renderPrint({

  print(stats_table())
     
   })
   
   
   
   
}




# Run the application 
shinyApp(ui = ui, server = server)

