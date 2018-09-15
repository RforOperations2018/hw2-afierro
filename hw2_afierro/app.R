# Class 5
# In Class Examples - Tabset

# Class 4
# In Class Examples - Inputs - Final

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

hw2dat <- read.csv("~/GitHub/hw2-afierro/CleanPittArressts.csv")
arrests.load <- hw2dat %>%

pdf(NULL)

# Define UI for application that draws a histogram
ui <- navbarPage("Pittsburgh Arrests", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Neighborhood select
                              selectInput("NeighborhoodSelect",
                                          "Neighborhood:",
                                          choices = sort(unique(hw2dat$Neighborhood),
                                                         multiple = TRUE,
                                                         selectize = TRUE,
                                                         selected = "Central Business District",
                              # Age Selection
                              sliderInput("ageSelect",
                                          "Age:",
                                              min = min(hw2dat$Age, na.rm = T),
                                              max = max(hw2dat$Age, na.rm = T),
                                              value = c(min(hw2dat$Age, na.rm = T), max(hw2dat$Age, na.rm = T)),
                                              step = 1),
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                                          ),
                              # Output plots
                              mainPanel(
                                plotlyOutput("Neighborhoodsplot"),
                                plotlyOutput("Ageplot")
                                          )
                              )
                            ),
# Data Table
tabPanel("Table",
    inputPanel(
      downloadButton("downloadData","Download Pittsburgh Arrests Data")
                                     ),
    fluidPage(DT::dataTableOutput("table"))
  )
)))
# Define server logic
server <- function(input, output, session = session) {
# Filtered Starwars data
    AInput <- reactive({
      arrests <- arrests.load %>%
# Age Slider Filter
    filter(Age >= input$ageSelect[1] & Age <= input$ageSelect[2])
      
# Neighborhood Filter
    if (length(input$NeighborhoodsSelect) > 0 ) {
    Ndat <- subset(hw2dat, Neighborhood %in% input$NeighborhoodSelect)
  }
                              
    return(Ndat)
})
# Reactive melted data
PAInput <- reactive({
      PInput() %>%
})
# Neighbrohood Plot
output$Neighborhoodsplot <- renderPlotly({
      dat <- PInput()
          ggplotly(
      ggplot(data = dat, aes(x = Neighborhood)) +                                                                                                   "<br>Height: ", height))) + 
      geom_bar()
})
# Age Plot
output$plot <- renderPlotly({
  dat <- PInput()
  ggplotly(
    ggplot(data = dat, aes(x = Age)) +                                                                                                   "<br>Height: ", height))) + 
  geom_bar()
})
# Data Table
output$table <- DT::renderDataTable({
  (hw2dat)
})
# Updating the URL Bar
      observe({
      print(reactiveValuesToList(input))
      session$doBookmark()
})
      onBookmarked(function(url) {
      updateQueryString(url)
})
# Download data in the datatable
      output$downloadData <- downloadHandler(
      filename = function() {
      paste("Pittarrests-data-", Sys.Date(), ".csv", sep="")
},
      content = function(file) {
      write.csv(swInput(), file)
}
  )
# Reset Filter Data
observeEvent(input$reset, {
      updateSelectInput(session, "NeighborhoodsSelect", selected = ("Central Business District")
      updateSliderInput(session, "AgeSelect", value = c(min(hw2dat$Age, na.rm = T), max(hw2dat$Age, na.rm = T)))
      showNotification("You have successfully reset the filters", type = "message")
    })
}
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")