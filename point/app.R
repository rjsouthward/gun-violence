#   -----
#   TO DO:
# Add *disclaimer at bottom of data table for why %'s don't add to 100. 
# Change labs to say; "Guns recovered IN/ Guns Recovered FROM X State.)
# Adjust point scaling. 
#   -----

library(shiny)
#For collapsable html panels
library(shinyBS)
library(tidyverse)
#Generate U.S map geoms
library(urbnmapr)
#Plot annotations
library(ggtext)
#Interactive Plots
library(plotly)

#Import data
data <- read_rds("data/combined2010to2020.rds") |>
  #Change 'Total' (name attributed by ATF) to United States throughout df
  mutate(Recovery_State = if_else(Recovery_State == 'Total', 'United States', Recovery_State)) |>
  mutate(Source_State = if_else(Source_State == 'Total', 'United States', Source_State)) |>
  #Filter out Guam and Virigin Islands because of lack of available population data.
  filter(! Source_State %in% c("Guam", "Virgin Islands")) |>
  filter(! Recovery_State %in% c("Guam", "Virgin Islands", "Canada")) |>
  #Remove topline summary of total guns recovered in U.S
  filter(!(Recovery_State == 'United States' & Source_State == 'United States'))

#Adjust gun data to be per 100k people. 
pop_data <- read_rds("data/state-populations-2010-2020.rds") |>
  pivot_longer(cols = `2010`:`2020`, names_to = "Year", values_to = "Population") |>
  mutate(Year = as.numeric(Year)) |>
  as.data.frame()

inflow_data <- left_join(data, pop_data, by = c('Source_State' = 'State', 'Year' = 'Year')) |>
  mutate(Guns_Recovered_PerCapita = (Guns_Recovered * 100000) / Population)

outflow_data <- left_join(data, pop_data, by = c('Recovery_State' = 'State', 'Year' = 'Year')) |>
  mutate(Guns_Recovered_PerCapita = (Guns_Recovered * 100000) / Population)

#Render state geoms
map <- get_urbn_map(map = 'territories_states') |>
  #Filter out states with no available data. 
  filter(! state_name %in% c("American Samoa", "Guam", "Mariana Islands", "Virgin Islands"))

stateList <- state.name
stateList <- append(stateList, 'Puerto Rico')

ui <- fluidPage(
  navbarPage(
  "Gun FlowR",
  tabPanel("Portal", fluidRow(
    column(4,
           radioButtons(inputId = "inout", "Gun inflow or outflow?",
                        choices = list("Inflow" = 1, "Outflow" = 2), selected = 1),
           selectInput(inputId = "state",
                       label = "Select state:",
                       choices = as.list(unique(data$Recovery_State)),
                       selected = list('United States')),
           selectInput(inputId = "year",
                       label = "Select year:",
                       choices = as.list(seq(from = 2010, to = 2020)),
                       selected = list(2020)),
           numericInput(inputId = "numStates",
                        label = "Select number of states/territories to return:",
                        min = 1,
                        max = length(unique(data$Recovery_State)),
                        value = 8),
           checkboxInput(inputId = "perCapita",
                         label = "Adjust guns recovered to be per 100k people in Source state (suggested)",
                         value = TRUE),
           checkboxInput(inputId = "excludeRecovery",
                         label = "Exclude recovery state from visualization for scale (suggested)",
                         value = TRUE)
    ),
    column(8,
           plotOutput(outputId = "plot")
    )
  ),
  fluidRow(
    column(12,
           bsCollapsePanel(
             title = "Detailed gun flow data ⌄", 
             dataTableOutput(outputId = 'table'),
             downloadButton("downloadData", "Download"),
             p(""),
             p("*Disclaimers:"),
             p("1. Percentages will not sum to 100% if less than 50 states are returned, and U.S territories are excluded."),
             p("2. Per capita statistics are calculated in terms of the source state."),
             p("3. All Data is sourced from the ATF Gun Summary Statistics.")
           )
    )
  )),
  tabPanel("Action", "filler"),
  tabPanel("About", "filler")
  )
)

server <- function(input, output){
  gunFlowData <- reactive({
    toArrange <- NULL
    #Gun inflow (what states do guns come FROM). 
    if (input$inout == 1) {
      i <- inflow_data |>
        filter(Source_State != 'United States') |>
        filter(Recovery_State == input$state, Year == input$year) |>
        mutate(Guns_Recovered_Pct = Guns_Recovered / sum(Guns_Recovered)) |>
        mutate(Guns_Recovered_Pct_PerCapita = Guns_Recovered_PerCapita / sum(Guns_Recovered_PerCapita)) |>
        select(Year, Recovery_State, Source_State, Guns_Recovered, Guns_Recovered_Pct, Guns_Recovered_PerCapita, Guns_Recovered_Pct_PerCapita)
        
      toArrange <- left_join(i, get_urbn_labels(map = 'territories'), by = c('Source_State' = 'state_name'))
    } else {
      #Gun outflow (what states do guns go TO).
      o <- outflow_data |>
        filter(Source_State == input$state, Year == input$year) |>
        mutate(Guns_Recovered_Pct = Guns_Recovered / sum(Guns_Recovered)) |>
        mutate(Guns_Recovered_Pct_PerCapita = Guns_Recovered_PerCapita / sum(Guns_Recovered_PerCapita)) |>
        select(Year, Recovery_State, Source_State, Guns_Recovered, Guns_Recovered_Pct, Guns_Recovered_PerCapita, Guns_Recovered_Pct_PerCapita)
      
      toArrange <- left_join(o, get_urbn_labels(map = 'territories'), by = c('Recovery_State' = 'state_name'))
    }
    #Determine if data should be arranged by per capita or raw values. 
    if (input$perCapita) {
      toArrange <- toArrange |> arrange(desc(Guns_Recovered_PerCapita))
    } else {
      toArrange <- toArrange |> arrange(desc(Guns_Recovered))
    }
    toArrange
  })
  output$plot <- renderPlot({
      #gunFlowData() is the data stored in reactive expression
      viz_data <- gunFlowData()
      
      #Determine whether or not to exclude Recovery State 
      if (input$excludeRecovery) {
        viz_data <- viz_data |> filter(Source_State != Recovery_State)
      }
      #Determine if data should be arranged by per capita or raw values. 
      sizingCol <- ''
      legendText <- ''
      if (input$perCapita) {
        sizingCol = 'Guns_Recovered_PerCapita'
        legendText = 'per 100k people'
      } else {
        sizingCol = 'Guns_Recovered'
      }
      viz_data <- viz_data |> head(input$numStates)
      
      #Determine how to color map (color entire U.S or one state)
      if (input$state == 'United States') {
        map <- map |> mutate(is_colored = if_else(state_name %in% stateList, TRUE, FALSE))
      } else {
        map <- map |> mutate(is_colored = if_else(state_name == input$state, TRUE, FALSE))
      }
      
      #Create plot
      plot <- ggplot() +
        geom_polygon(data = map, aes(x = long, y = lat, group = group, fill = is_colored), color = '#ffffff') +
        scale_fill_manual(values = c('#C0C0C0', '#AF251F')) +
        geom_point(data = viz_data, aes_string(x = 'long', y = 'lat', size = sizingCol)) +
        scale_size_area() +
        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
        theme(panel.background = element_blank(), 
              axis.title = element_blank(), 
              axis.text = element_blank(), 
              axis.ticks = element_blank(), 
              legend.key = element_blank(), 
              plot.caption = element_text(hjust = 0)) +
        guides(fill = "none") +
        labs(title = paste("Origins of Guns Recovered from", input$state, "in", input$year),
             size = paste("Guns Sourced", legendText, sep = ' '), 
             caption = "Source: Bureau of Alcohol, Tobacco, Firearms and Explosives (ATF)")
      
      #ggplotly(plot)
      plot
  })
  
  output$table <- renderDataTable({
    gunFlowData() |> 
      select(-lat, -long, -state_abbv) |>
      head(input$numStates)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(gunFlowData(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
