library(shiny)
library(tidyverse)
library(rnaturalearth)
library(geosphere)
library(leaflet)
library(RColorBrewer)

setwd("~/Desktop/Personal-Projects/gun-violence")

states <- ne_states(iso_a2 = 'US') 
scale_factor <- 10

names <- sort(states@data$name)
data <- read_rds("data/consolidated/combined2010to2020.rds") |>
  filter(Source_State != Recovery_State) |>
  group_by(Year, Recovery_State) |>
  filter(Guns_Recovered > quantile(Guns_Recovered, probs = .8)) |>
  ungroup() |>
  mutate(Guns_Recovered = Guns_Recovered / scale_factor)

coords <- states@data |> select(name, longitude, latitude) 

# Rename lat and long cols to reference Source/Recovery. Makes merge easier later on.  
Recovery_coords <- coords |> rename(Recovery_long = longitude, Recovery_lat = latitude)
Source_coords <- coords |> rename(Source_long = longitude, Source_lat = latitude)

# Cross reference - start with Source/Recovery State name, and compare to name in coords

mainland <- data |> 
  filter(Recovery_State %in% names) |>
  filter(Source_State %in% names)

mainland <- left_join(mainland, Recovery_coords, by = c("Recovery_State" = "name"))
mainland <- left_join(mainland, Source_coords, by = c("Source_State" = "name"))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "state",
                  label = "Select state:",
                  choices = as.list(names)),
      selectInput(inputId = "year",
                  label = "Select year:",
                  choices = as.list(seq(from = 2010, to = 2020))),
      submitButton("Submit")
    ),
    mainPanel(
      leafletOutput(outputId = "recoStateTop10")
    )
  )
)

server <- function(input, output){
  output$recoStateTop10 <- renderLeaflet({
    
    filtered <- mainland |>
      filter(Recovery_State == input$state) |>
      filter(Year == input$year)

    flows <- gcIntermediate(filtered[,5:6], filtered[,7:8], sp = TRUE, addStartEnd = TRUE)
    flows$Recovery_State <- filtered$Recovery_State
    flows$Source_State <- filtered$Source_State
    flows$Guns_Recovered <- filtered$Guns_Recovered
    
    # Create leaflet graph
    
    #origins
    hover <- paste0(flows$Source_State, " to ", 
                    #destinations
                    flows$Recovery_State, ': ', 
                    as.character(flows$Guns_Recovered*scale_factor))
    
    pal <- colorFactor(brewer.pal(4, 'Set2'), flows$Source_State)
    
    leaflet(flows) |>
      addProviderTiles('CartoDB.Positron') |>
      addPolylines(weight = ~Guns_Recovered, label = hover, 
                   group = ~Source_State, color = ~pal(Source_State)) |>
      addLayersControl(overlayGroups = unique(flows$Source_State), 
                       options = layersControlOptions(collapsed = FALSE))
    
  })
}

shinyApp(ui = ui, server = server)
