#Homework 2
#Allyson Fierro

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

hw2dat <- read.csv("~/GitHub/hw2-afierro/CleanPittArressts.csv")

pdf(NULL)

ui <- navbarPage("Pittsburgh Arrests", 
                 tabPanel("Plot_1",
                          sidebarLayout(
                            sidebarPanel(
                              # Arrest select
                              selectInput("ArrestSelect",
                                          "Arrest:",
                                          choices = c("Criminal Conspiracy", "Criminal Mischief", "Forgery", "Marijuana Possession", "Retail Theft", "Public Drunkenness"),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = ("Criminal Conspiracy"))
                              
                            ))))

server <- function(input, output, session = session) {
  output$plot <- renderPlotly({
    ggplot(data = dat, aes(x = Arrest1 )) + geom_bar()
  })
}

shinyApp(ui = ui, server = server, session = session)

