# Let's change the headers next time. Easy points were missed.

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

# I had to move this file to the directory the app was in for it to load and change the
hw2dat <- read.csv("CleanPittArressts.csv")
arrests.load <- hw2dat # %>% 
# The above above was breaking things.One of the first reasons your app wasn't working

pdf(NULL)

# Define UI for application that draws a histogram
ui <- navbarPage("Pittsburgh Arrests", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Neighborhood select
                              selectInput("NeighborhoodSelect",
                                          "Neighborhood:",
                                          # Missing close paren
                                          choices = sort(unique(arrests.load$Neighborhood)),
                                                         multiple = TRUE,
                                                         selectize = TRUE,
                                                         selected = c("Central Business District", "Shadyside", "North Shore")),
                              # Race select
                              selectInput("RaceSelect",
                                          "Race:",
                                          # Missing close paren
                                          choices = sort(unique(arrests.load$Race)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = "White"),
                              # Age Selection
                              sliderInput("ageSelect",
                                          "Age:",
                                              min = min(arrests.load$Age, na.rm = T),
                                              max = max(arrests.load$Age, na.rm = T),
                                              value = c(min(arrests.load$Age, na.rm = T), max(arrests.load$Age, na.rm = T)),
                                              step = 1),
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                                          ),
                              # Output plots
                              mainPanel(
                                plotlyOutput("Neighborhoodsplot"),
                                plotlyOutput("Raceplot"),
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
))

# Define server logic
server <- function(input, output, session = session) {
# Filtered arrests data
    PInput <- reactive({
      arrests <- arrests.load %>%
# Age Slider Filter
      filter(Age >= input$ageSelect[1] & Age <= input$ageSelect[2])
      
# Neighborhood Filter
    if (length(input$NeighborhoodsSelect) > 0 ) {
    # If the neighborhood select is zero, you end up calling a variable that doesn't exist. Unlike other instances in R, in Shiny we want to keep overwriting the object because of issues like this.
    arrrests <- subset(arrests, Neighborhood %in% input$NeighborhoodSelect)
  }
                              
    return(arrests)
})
# Reactive data
PAInput <- reactive({
      PInput() #%>% pipes are only needed if you're doing something
})
# Neighbrohood Plot
output$Neighborhoodsplot <- renderPlotly({
      dat <- PInput()
      ggplot(data = dat, aes(x = Neighborhood)) + 
      geom_bar()
})
# Race Plot
output$Raceplot <- renderPlotly({
  # You called it dat but then called dat.race, be consistent with your naming of objects
  dat.race <- PInput()
  # Race2 isn't a column, changed it to Race
  ggplot(data = dat.race, aes(x = Race, fill = Arrest1)) + 
    geom_bar() +
    guides(color = FALSE)
})
# Age Plot
output$plot <- renderPlotly({
  dat <- PInput()
  # Height isn't a variable in this data.
    ggplot(data = dat, aes(x = Age)) + 
  geom_bar() +
  guides(color = FALSE)
})
# Data Table
output$table <- DT::renderDataTable({
  # This isn't tied to the reactive data
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
      updateSelectInput(session, "NeighborhoodsSelect", selected = c("Central Business District", "Shadyside", "North Shore"))
      updateSelectInput(session, "RaceSelect", selected = "White")
      updateSliderInput(session, "AgeSelect", value = c(min(hw2dat$Age, na.rm = T), max(hw2dat$Age, na.rm = T)))
      showNotification("You have successfully reset the filters", type = "message")
    })
}
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")