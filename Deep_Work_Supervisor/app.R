#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
library(tidyverse)

daily_all <- read_csv("daily_all.csv")
trend_type <- read_csv("trend_type.csv")

# Define UI for application that draws a histogram
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
         plotOutput("line_plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
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
}

# Run the application 
shinyApp(ui = ui, server = server)

