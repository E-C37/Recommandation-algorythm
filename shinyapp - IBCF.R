#libraries we used
library(shiny)
library(shinythemes)

#R Script Source
source("IBCF-final.R")

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel("Netflix Movie Recommendation"),
                sidebarPanel(numericInput("id", "UserID:","", min = 1, max = 100000, value = 0),
                             br(),
                             actionButton("click", "Search"),
                             p("Click to update the recommendations displayed in the main panel.")),
                mainPanel(tableOutput(outputId = "Recommendations2")))

#Server
shinyServer <-function(input, output) {
  output$Recommendations2 <- renderTable({
    recommendIBCF(input$id)
  })
}
# Run the application 
shinyApp(ui = ui, server = shinyServer)
