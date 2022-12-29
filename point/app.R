library(shiny)
#For collapsable html panels
library(shinyBS)
library(tidyverse)
#Generate U.S map geoms
library(urbnmapr)

#Import data
#setwd("~/Desktop/Personal-Projects/gun-violence")
data <- read_rds("combined2010to2020.rds") |>
  #Change 'Total' (name attributed by ATF) to United States throughout df
  mutate(Recovery_State = if_else(Recovery_State == 'Total', 'United States', Recovery_State)) |>
  mutate(Source_State = if_else(Source_State == 'Total', 'United States', Source_State)) |>
  #Filter out Guam and Virigin Islands because of lack of available population data.
  filter(! Source_State %in% c("Guam", "Virgin Islands")) |>
  filter(! Recovery_State %in% c("Guam", "Virgin Islands"))

#Adjust gun data to be per 100k people. 
pop_data <- read_rds("state-populations-2010-2020.rds") |>
  pivot_longer(cols = `2010`:`2020`, names_to = "Year", values_to = "Population") |>
  mutate(Year = as.numeric(Year)) |>
  as.data.frame()

data <- left_join(data, pop_data, by = c('Source_State' = 'State', 'Year' = 'Year')) |>
  mutate(Guns_Recovered_PerCapita = (Guns_Recovered * 100000) / Population)
  

ui <- fluidPage(
  fluidRow(
    column(4,
      selectInput(inputId = "state",
                  label = "Select state:",
                  choices = as.list(unique(data$Source_State)),
                  selected = list('United States')),
      selectInput(inputId = "year",
                  label = "Select year:",
                  choices = as.list(seq(from = 2010, to = 2020)),
                  selected = list(2020)),
      numericInput(inputId = "numStates",
                  label = "Select number of states/territories to return:",
                  min = 0,
                  max = length(unique(data$Recovery_State)),
                  value = 8),
      submitButton("Submit")
    ),
    column(8,
      plotOutput(outputId = "plot")
    ), 
  ),
  fluidRow(
    column(12,
      bsCollapsePanel(
        "Detailed gun flow data ⌄", 
        dataTableOutput(outputId = 'table')
      )
    )
  )
    
)

server <- function(input, output){
  dataInput <- reactive({
    if (input$state == 'United States'){
      data |>
        filter(Source_State != 'United States') |>
        group_by(Recovery_State, Year) |>
        arrange(desc(Guns_Recovered)) |>
        slice(1:input$numStates) |>
        filter(Recovery_State == input$state, Year == input$year) |>
        mutate(Guns_Recovered_Pct = Guns_Recovered / sum(Guns_Recovered)) |>
        mutate(Guns_Recovered_Pct_PerCapita = Guns_Recovered_PerCapita / sum(Guns_Recovered_PerCapita)) |>
        select(Year, Recovery_State, Source_State, Guns_Recovered, Guns_Recovered_Pct, Guns_Recovered_PerCapita, Guns_Recovered_Pct_PerCapita)
      #display guns recovered from an individual state
    } else {
      data |>
        filter(Source_State != 'United States') |>
        filter(Recovery_State == input$state, Year == input$year) |>
        group_by(Recovery_State, Year) |>
        arrange(desc(Guns_Recovered)) |>
        slice(1:input$numStates) |>
        mutate(Guns_Recovered_Pct = Guns_Recovered / sum(Guns_Recovered)) |>
        mutate(Guns_Recovered_Pct_PerCapita = Guns_Recovered_PerCapita / sum(Guns_Recovered_PerCapita)) |>
        select(Year, Recovery_State, Source_State, Guns_Recovered, Guns_Recovered_Pct, Guns_Recovered_PerCapita, Guns_Recovered_Pct_PerCapita)
    }
  })
  output$plot <- renderPlot({
    #display states that source the most guns across the entire US
    #should check to make sure that I'm not counting guns sourced and recovered in the same state
    if (input$state == 'United States'){
      filtered <- data |>
        #why does Total still show up?
        filter(Recovery_State == 'United States', Source_State != 'United States', Source_State != 'Total') |>
        pivot_wider(names_from = Year, values_from = Guns_Recovered, names_prefix = 'Y') |>
        arrange(desc(!!sym(paste('Y', input$year, sep = ''))))
      
      combined <- left_join(filtered, get_urbn_labels(map = 'territories'), by = c('Source_State' = 'state_name')) |>
        arrange(desc(paste('Y', input$year, sep = ''))) |>
        head(input$numStates)
      
      map <- get_urbn_map(map = 'territories_states') |>
        #Filter out states with no available data. 
        filter(! state_name %in% c("American Samoa", "Guam", "Mariana Islands", "Virgin Islands"))
      
      ggplot() +
        geom_polygon(data = map, aes(x = long, y = lat, group = group), color = '#ffffff', fill = '#AF251F', size = .25) +
        geom_point(data = combined, aes_string(x = 'long', y = 'lat', size = paste('Y', input$year, sep = '')), na.rm = TRUE) +
        #scale_size(range = c(4, 9)) +
        scale_size_area() +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        theme(panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.key=element_blank(), plot.caption = element_text(hjust = 0)) +
        guides(fill = "none") +
        labs(title = paste("Origins of Guns Recovered from", input$state, "in", input$year),
             size = "Guns Sourced", 
             caption = "Source: Bureau of Alcohol, Tobacco, Firearms and Explosives (ATF)") 
      
      
    #display guns recovered from an individual state
    } else {
      filtered <- data |>
        filter(Recovery_State != 'United States') |>
        filter(Source_State != 'United States') |>
        #filter(Source_State != Recovery_State) |>
        group_by(Recovery_State, Year) |>
        arrange(desc(Guns_Recovered)) |>
        slice(1:input$numStates) |>
        ungroup() |>
        pivot_wider(names_from = Year, values_from = Guns_Recovered, names_prefix = 'Y')
      
      combined <- left_join(filtered, get_urbn_labels(map = 'territories'), by = c('Source_State' = 'state_name')) |>
        filter(Recovery_State == input$state) 
      
      # Obtain state polygons (urbnmapr package)
      map <- get_urbn_map(map = 'territories_states') |>
        # Create new column to determine if state should be filled
        mutate(is_colored = factor(if_else(state_name == input$state, 1, 0)))
      
      print(combined)
      ggplot() +
        geom_polygon(data = map, aes(x = long, y = lat, group = group, fill = is_colored), color = '#ffffff') +
        scale_fill_manual(values = c('#C0C0C0', '#AF251F')) +
        geom_point(data = combined, aes_string(x = 'long', y = 'lat', size = paste('Y', input$year, sep = '')), na.rm = TRUE) +
        scale_size_area() +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        theme(panel.background = element_blank(), 
              axis.title = element_blank(), 
              axis.text = element_blank(), 
              axis.ticks = element_blank(), 
              legend.key=element_blank(), 
              plot.caption = element_text(hjust = 0)) +
        guides(fill = "none") +
        labs(title = paste("Origins of Guns Recovered from", input$state, "in", input$year),
             size = "Guns Sourced", 
             caption = "Source: Bureau of Alcohol, Tobacco, Firearms and Explosives (ATF)") 
    }
    
  })
  
  output$table <- renderDataTable({
    dataInput()
  })
}

shinyApp(ui = ui, server = server)
