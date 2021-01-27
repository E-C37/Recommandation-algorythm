#libraries we used
library(shiny)
library(shinythemes)

#R Script Source
source("Association Rule.R")

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel("Netflix Movie Recommendation"),
                p("Insert your UserID"),
                sidebarPanel(numericInput("id", "UserID:","", min = 1, max = 100000, value = 0),
                             br()),
                mainPanel(h3(strong("Polina")),
                          tableOutput("Recommendations3")))
#Server
shinyServer <-function(input, output) {
  output$Recommendations3 <- renderTable({
    myfunction(input$id)
  })
}

# Run the application 
shinyApp(ui = ui, server = shinyServer)
