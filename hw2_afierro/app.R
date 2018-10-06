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

Race <- sort(ckanUniques("e03a89dd-134a-4ee8-a2bd-62c40aeebc6f", "INCIDENTNEIGHBORHOOD")$INCIDENTNEIGHBORHOOD)
Neighborhood <- sort(ckanUniques("e03a89dd-134a-4ee8-a2bd-62c40aeebc6f", "RACE")$RACE)
Age <- sort(ckanUniques("e03a89dd-134a-4ee8-a2bd-62c40aeebc6f", "AGE")$AGE)

dat <- ckanSQL("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22e03a89dd-134a-4ee8-a2bd-62c40aeebc6f%22%20WHERE%22OFFENSES%22%20LIKE%20%27%Public%20Drunk%%27") 
df <- dat %>%
  rename(Arrest = OFFENSES) %>%
  rename(Neighborhood = INCIDENTNEIGHBORHOOD) %>%
  rename(Race = RACE) %>%
  rename(Age = AGE)

pdf(NULL)

# Define UI for application that draws a histogram
ui <- navbarPage("Pittsburgh Arrests", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Neighborhood select
                              selectInput("NeighborhoodSelect",
                                          "Neighborhood:",
                                          choices = Neighborhood,
                                          selected = "Central Business District"),
                              # Race select
#                              selectInput("RaceSelect",
#                                          "Race:",
#                                          choices = Race,
#                                          selected = "White"),
                              # Age Selection
                              sliderInput("ageSelect",
                                          "Age:",
                                              min = min (Age, na.rm = T),
                                              max = max(Age, na.rm = T),
                                              value = c(min(Age, na.rm = T), max(Age, na.rm = T)),
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
      
      url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22e03a89dd-134a-4ee8-a2bd-62c40aeebc6f%22%20WHERE%20%22CREATED_ON%22%20%3E=%20%27", input$ageSelect[1], "%27%20AND%20%22CREATED_ON%22%20%3C=%20%27", input$ageSelect[2], "%27%20AND%20%22Neighborhood%22%20=%20%27", input$NeighborhoodSelect, "%27")
      
      arrests <- ckanSQL(url) %>%
# Age Slider Filter
      filter(Age >= input$ageSelect[1] & Age <= input$ageSelect[2])
      
# Neighborhood Filter
    if (length(input$NeighborhoodSelect) > 0 ) {
      url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22e03a89dd-134a-4ee8-a2bd-62c40aeebc6f%22%20WHERE%20%22CREATED_ON%22%20%3E=%20%27", input$ageSelect[1], "%27%20AND%20%22CREATED_ON%22%20%3C=%20%27", input$ageSelect[2], "%27%20AND%20%22Neighborhood%22%20=%20%27", input$NeighborhoodSelect, "%27")
      arrests <- ckanSQL(url)
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
  ggplot(data = dat.race, aes(x = Race)) + 
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
(data = df)
  
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
      updateSelectInput(session, "NeighborhoodsSelect", selected = "Central Business District")
      updateSelectInput(session, "RaceSelect", selected = "White")
      updateSliderInput(session, "AgeSelect", value = c(min(Age, na.rm = T), max(Age, na.rm = T)))
      showNotification("You have successfully reset the filters", type = "message")
    })
}
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")