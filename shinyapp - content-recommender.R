#libraries we used
library(shiny)
library(shinythemes)

#R Script Source
source("content-recommender.R")

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel("Netflix Movie Recommendation"),
                sidebarPanel(numericInput("id", "UserID:","", min = 1, max = 100000, value = 0),
                             br(),
                             actionButton("click", "Search"),
                             p("Click to update the recommendations displayed in the main panel.")),
                mainPanel(tableOutput(outputId = "Recommendations1")))

#Server
shinyServer <-function(input, output) {
  output$Recommendations1 <- renderTable({
    recommend2(input$id)
  })
}
# Run the application 
shinyApp(ui = ui, server = shinyServer)
