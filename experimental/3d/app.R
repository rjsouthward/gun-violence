library(shiny)
library(tidyverse)
library(usmap)
library(rgl)
library(rayshader)
library(sf)
library(cdlTools)

setwd("~/Desktop/Personal-Projects/gun-violence")
options(rgl.useNULL = FALSE)

data <- read_rds("data/consolidated/combined2010to2020.rds")

filtered1 <- data |>
  filter(! Recovery_State %in% c("VIRGIN ISLANDS", "TOTAL", "GUAM", "CANADA")) |>
  filter(Source_State != "TOTAL") |>
  filter(Recovery_State != Source_State) |>
  group_by(Year, Recovery_State) |>
  filter(Guns_Recovered > quantile(Guns_Recovered, probs = .9)) |>
  ungroup() |>
  pivot_wider(names_from = Year, values_from = Guns_Recovered, values_fill = 0) |>
  arrange(Recovery_State) |>
  mutate("Source_FIPS" = cdlTools::fips(Source_State, to = "FIPS")) |>
  
  #cdlTools does not include Virgin Islands FIPS, so do it manually. 
  mutate(Source_FIPS = ifelse(Source_State == "VIRGIN ISLANDS", 78, Source_FIPS))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "state",
                  label = "Select state:",
                  choices = as.list(unique(filtered1$Recovery_State))),
      selectInput(inputId = "year",
                  label = "Select year:",
                  choices = as.list(seq(from = 2010, to = 2020))),
      submitButton("Submit")
    ),
    mainPanel(
      plotOutput(outputId = "recoStateTop10")
    )
  )
)

server <- function(input, output){
  output$recoStateTop10 <- renderPlot({
    
    filtered2 <- filtered1 |>
      filter(Recovery_State == input$state) |>
      select(Source_FIPS, as.character(input$year)) |>
      rename("fips" = "Source_FIPS", "values" = as.character(input$year))
    
    map <- plot_usmap(data = filtered2, regions = "states") +
      scale_fill_viridis_c()
    
    plot_gg(map, shadow_intensity = .85, offset_edges = TRUE, raytrace = TRUE, sunangle = 135, solidcolor = "ghostwhite", 
            theta = 0, phi = 35)
    
    render_snapshot(clear = TRUE)
  })
}

shinyApp(ui = ui, server = server)
