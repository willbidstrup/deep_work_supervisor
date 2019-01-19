

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
selectInput("category", "Select a category", choices = c("Work", "Craft", "Study", "Wealth", "Other"))
      ),
      
      # Show a plot
      mainPanel(
         plotOutput("line_plot"),
         verbatimTextOutput("summary_stats"),
         plotOutput("box_plot")
      )
   )
)


########################## SERVER ##############################


server <- function(input, output) {
   
  ## Reactive variables
  
  stats_table <- reactive({
    summary(trend_type %>% 
              filter(category == input$category) %>%
              select(time_spent))
    
  })
  
  
  ## PLOT 1
   output$line_plot <- renderPlot({
      # generate plot
      x <- trend_type
      # draw the plot
      ggplot(data = x %>%
               filter(category == input$category), aes(x = Date, y = time_spent)) +
        geom_point(alpha = 0.2) +
        geom_smooth() +
        ylim(0, 6) +
        xlim(min(x$Date), max(x$Date)) +
        theme_minimal()
   })
   
   
   ## SUMMARY TEXT
   output$summary_stats <- renderPrint({

  print(stats_table())
     
   })
   
   
   
   ## PLOT 2
   output$box_plot <- renderPlot({
     # generate plot
     x <- trend_type
     # draw the plot
     ggplot(data = x, aes(x = category, y = time_spent, col = category)) +
       geom_boxplot(alpha = 0.5) +
       ylim(0, 6) +
       theme_minimal()

     
   })
   
   
   
   
}




# Run the application 
shinyApp(ui = ui, server = server)

