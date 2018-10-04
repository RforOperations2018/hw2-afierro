# Let's change the headers next time. Easy points were missed.

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(httr)
library(jsonlite)
library(htmltools)

ckanSQL <- function(url) {
  # Make the Request
  r <- GET(url)
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

races <- sort(ckanUniques("e03a89dd-134a-4ee8-a2bd-62c40aeebc6f", "INCIDENTNEIGHBORHOOD")$INCIDENTNEIGHBORHOOD)
neighborhoods <- sort(ckanUniques("e03a89dd-134a-4ee8-a2bd-62c40aeebc6f", "RACE")$RACE)

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
                                plotlyOutput("plot")
                               
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
    if (length(input$NeighborhoodSelect) > 0 ) {
    # If the neighborhood select is zero, you end up calling a variable that doesn't exist. Unlike other instances in R, in Shiny we want to keep overwriting the object because of issues like this.
    arrests <- subset(arrests, Neighborhood %in% input$NeighborhoodSelect)
  }
                              
    return(arrests)
})
# Neighbrohood Plot
output$Neighborhoodsplot <- renderPlotly({
      dat <- PInput()
      ggplot(data = dat, aes(x = Neighborhood)) + 
      geom_bar()
})
# Race Plot
output$Raceplot <- renderPlotly({
  dat.race <- PInput()
  ggplot(data = dat.race, aes(x = Race, fill = Arrest1)) + 
    geom_bar() +
    guides(color = FALSE)
})
# Age Plot
output$plot <- renderPlotly({
  dat <- PInput()
    ggplot(data = dat, aes(x = Age)) + 
  geom_bar() +
  guides(color = FALSE)
})
# Data Table
output$table <- DT::renderDataTable({
  # This isn't tied to the reactive data
  PInput()
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
      write.csv(PInput(), file)
}
  ) 
# Reset Filter Data
observeEvent(input$reset, {
      updateSelectInput(session, "NeighborhoodsSelect", selected = c("Central Business District", "Shadyside", "North Shore"))
      updateSelectInput(session, "RaceSelect", selected = "White")
      updateSliderInput(session, "AgeSelect", value = c(min(arrests.load$Age, na.rm = T), max(arrests.load$Age, na.rm = T)))
      showNotification("You have successfully reset the filters", type = "message")
    })
}
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")