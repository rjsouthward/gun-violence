library(tidyverse)
library(urbnmapr)

#Import data
setwd("~/Desktop/Personal-Projects/gun-violence")
data <- condensed |>
  #Change 'Total' (name attributed by ATF) to United States throughout df
  mutate(Recovery_State = if_else(Recovery_State == 'Total', 'United States', Recovery_State)) |>
  mutate(Source_State = if_else(Source_State == 'Total', 'United States', Source_State))

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
      dataTableOutput(outputId = 'table')
    )
  )
    
)

server <- function(input, output){
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
      
      map <- get_urbn_map(map = 'territories_states') 
      
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
        filter(Source_State != Recovery_State) |>
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
    if (input$state == 'United States'){
      
      filtered <- data |>
        #why does Total still show up?
        filter(Recovery_State == 'United States') |>
        filter(Source_State != 'United States') |>
        filter(Source_State != 'Total') |>
        pivot_wider(names_from = Year, values_from = Guns_Recovered, names_prefix = 'Y') 
      
      table <- left_join(filtered, get_urbn_labels(map = 'territories'), by = c('Source_State' = 'state_name')) |>
        arrange(desc(!!sym(paste('Y', input$year, sep = '')))) |>
        slice(1:input$numStates) |>
        select(Recovery_State,Source_State, paste('Y', input$year, sep = '')) |>
        rename('Recovery State' = Recovery_State,'Source State' = Source_State)
      
      table
      
      #display guns recovered from an individual state
    } else {
      filtered <- data |>
        filter(Recovery_State != 'United States') |>
        filter(Source_State != 'United States') |>
        filter(Source_State != Recovery_State) |>
        group_by(Recovery_State, Year) |>
        arrange(desc(Guns_Recovered)) |>
        slice(1:input$numStates) |>
        ungroup() |>
        pivot_wider(names_from = Year, values_from = Guns_Recovered, names_prefix = 'Y')
      
      combined <- left_join(filtered, get_urbn_labels(map = 'territories'), by = c('Source_State' = 'state_name')) |>
        filter(Recovery_State == input$state) |>
        select(Recovery_State, Source_State, paste('Y', input$year, sep = '')) |>
        drop_na() |>
        rename('Recovery State' = Recovery_State,'Source State' = Source_State)
      
      combined
    }
  })
}

shinyApp(ui = ui, server = server)
