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

daily_all <- read_csv("../data_transformed/daily_all.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   h1("Deep Work Supervisor - Test"),
   # Application title
   titlePanel(""),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
"The Deep Work Supervisor app will show improved statistics on intervals from the BeFocused app."
      ),
      
      # Show a plot
      mainPanel(
         plotOutput("density_plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$density_plot <- renderPlot({
      # generate plot
      x <- daily_all
      # draw the plot
      ggplot(data = x, aes(x = Date, y = focused_prop)) +
        geom_point() +
        geom_smooth() +
        theme_minimal() +
        labs(title = "Trend of focused time as a proportion of total time")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

