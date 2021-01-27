#libraries we used
library(arules)
library(anytime)
library(stringr)
library(caTools)
library(cluster)
library(clustMixType)
library(coop)
library(data.table)
library(dplyr)
library(ggplot2)
library(graphics)
library(grid)
library(gridExtra)
library(kableExtra)
library(klaR)
library(knitr)
library(lattice)
library(lubridate)
library(plyr)
library(readr)
library(recommenderlab)
library(shiny)
library(shinythemes)
library(tidyr)
library(tidyverse)


#R Script Sources
source("content-recommender.R")
source("IBCF-final.R")
source("Association Rule.R")



# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel(h1(strong("Netflix Movie Recommendation"))),
                sidebarPanel(p(h4("Sign In")),
                             numericInput("id", "UserID:", min = 1, max = 125557, value = 0, step = 1, width = NA),
                             br()),
                mainPanel(column(4,h3(strong("Familiar Favourites in this Genre")),
                                 tableOutput("Recommendations1")),
                          column(4,h3(strong("Our Top Picks for You")),
                                 tableOutput("Recommendations2")),
                          column(4,h3(strong("You Might Like These Movies Too")),
                                 tableOutput("Recommendations3"))),
                fluidRow(column(12,
                                helpText(strong("This project is part of TBS's MSc Big Data Marketing & Management Program - Class 2020/2021.")),
                                helpText(strong("Group 5 :")),
                                helpText("- CHOLLET Emilien"),
                                helpText("- HAAN SHABAROVA Polina"),
                                helpText("- LAKIM Kenza"),
                                helpText("- ROSSI Yohan"),
                                helpText("- SARKIS Wissam"),
                                helpText("- WANG Jia"),
                )))


#Server
shinyServer <-function(input, output) {
  output$Recommendations1 <- renderTable({
    recommend2(input$id)
  })
  output$Recommendations2 <- renderTable({
    recommendIBCF(input$id)
  })
  output$Recommendations3 <- renderTable({
    myfunction(input$id)
  })
}

# Run the application 
shinyApp(ui = ui, server = shinyServer)
