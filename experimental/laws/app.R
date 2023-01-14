library(shiny)
library(tidyverse)
library(heatmaply)

setwd('/Users/rsouthward/Desktop/Personal-Projects/gun-violence')

gunflow <- read_rds('data/consolidated/combined2010to2020.rds') |>
  #Change 'Total' (name attributed by ATF) to United States throughout df
  mutate(Recovery_State = if_else(Recovery_State == 'Total', 'United States', Recovery_State)) |>
  mutate(Source_State = if_else(Source_State == 'Total', 'United States', Source_State)) |>
  #Filter out Guam and Virigin Islands because of lack of available population data.
  filter(! Source_State %in% c("Guam", "Virgin Islands")) |>
  filter(! Recovery_State %in% c("Guam", "Virgin Islands")) |>
  filter(Source_State != "United States")

provisions <- read_rds('data/consolidated/state-provisions-2010-2018.rds') |>
  filter(Current_Status != "Repealed") |>
  select(-Current_Status)

# Include adjacent states:
state_adjacency <- read_rds("data/consolidated/state-adjacency.rds")

state_abbs <- state.abb
state_abbs <- append(state_abbs, "DC", after = 7)

state_names <- state.name
state_names <- append(state_names, "District of Columbia", after = 7)

abb_name_matching <- data.frame(abb = state_abbs, name = state_names)

prov_descs <- as.vector(unique(provisions$Provision_Description))
#Add missing description in data
prov_descs <- append(prov_descs, "No additional finding is required before the firearm surrender provisions apply", after = 62)

#save data frame of provision descriptions and names for matching later. 
matching <- data.frame(Provision_Name = as.vector(unique(provisions$Provision_Name)),
                       Provision_Description = prov_descs)

#provision data only goes up to 2018, so add junk data for years 2019 and 2020
state_names <- state.name
state_names <- append(state_names, c("District of Columbia", "Puerto Rico"))


all_provisions <- expand_grid(State_Name = state_names,
                              Year = 2010:2020,
                              Provision_Name = as.vector(unique(provisions$Provision_Name)))


#Create status codes for color coding.
provisions <- provisions |>
  #Drop Provision Descriptions and match them later.
  select(-Provision_Description) |>
  #This is incorrect: some states/years have no data, but we will correct later. 
  mutate(Status_Code = "Has Provision") |>
  #Fix years appearing as type char
  mutate(Year = as.integer(Year)) 
  
  all_provisions <- left_join(all_provisions, provisions, by = c("State_Name" = "State_Name", "Year" = "Year", "Provision_Name" = "Provision_Name")) |>
    #Add laws that states are missing to 'Status_Code'
    mutate(Status_Code = if_else(is.na(Status_Code), "Missing Provision", Status_Code)) |>
    #No data for PR or DOC. 
    mutate(Status_Code = if_else(State_Name %in% c("Puerto Rico", "District of Columbia"), "No Data", Status_Code)) |>
    #Data only goes up until Y2018.
    mutate(Status_Code = if_else(Year %in% 2019:2020, "No Data", Status_Code)) |>
    mutate(Status_Code = as.factor(Status_Code))
  
  provision_data <- left_join(gunflow, all_provisions, by = c("Source_State" = "State_Name", "Year" = "Year"))
  
  density <- provision_data |> 
    filter(Status_Code == "Missing Provision") |>
    group_by(Provision_Name) |>
    summarize(Total_Guns_Recovered = sum(Guns_Recovered)) |>
    arrange(desc(Total_Guns_Recovered)) |>
    rowid_to_column("Provision_Number")
  
  matching <- left_join(matching, density, by = c('Provision_Name' = 'Provision_Name')) |>
    select(-Total_Guns_Recovered)

ui <- fluidPage(
  fluidRow(
    column(12, 
           selectInput(inputId = "state",
                       label = "Select state:",
                       choices = as.list(unique(provision_data$Source_State)),
                       selected = list('Illinois')),
           selectInput(inputId = "year",
                       label = "Select year:",
                       choices = as.list(seq(from = 2010, to = 2020)),
                       selected = list(2018)),
           numericInput(inputId = "numStates",
                        label = "Select number of states/territories to return:",
                        min = 1,
                        max = length(unique(provision_data$Recovery_State)),
                        value = 8)
           )
  ),
  fluidRow(
    column(8, plotlyOutput("provisions")),
    column(4, plotlyOutput("gunTotals"))
  ),
  fluidRow(
    column(12, verbatimTextOutput("hover"))  
  )
)

server <- function(input, output) {
  provReactive <- reactive({
    
    ui_partial <- provision_data |>
      filter(Year == input$year, Recovery_State == input$state)
    
    adj_data <- left_join(state_adjacency, abb_name_matching, by = c("StateCode" = "abb"))
    adj_states <- left_join(adj_data, abb_name_matching, by = c("NeighborStateCode" ="abb")) |>
      select(3:4) |>
      rename("State" = "name.x", "Neighbor" = "name.y") |>
      filter(State == input$state) |>
      pull(Neighbor)
    
    
    #Filter to be the top n states
    reactive_data <- ui_partial |>
      arrange(desc(Guns_Recovered)) |>
      group_by(Recovery_State, Source_State, Guns_Recovered, Year) |>
      nest() |>
      head(input$numStates) |>
      ungroup() |>
      unnest() 
    
    #Add back adjacent state data
    reactive_data <- rbind(reactive_data, ui_partial |>
                             filter(Source_State %in% adj_states)) |>
      distinct() |>
      arrange(desc(Guns_Recovered)) 
    
    #Reactive output
    left_join(reactive_data, matching, by = c("Provision_Name" = "Provision_Name"))
  })
  
  clickData <- reactive({
    event_data("plotly_click")
  })
  
  output$provisions <- renderPlotly({
    
    provision_matrix <- provReactive() |>
      select(Provision_Number, Source_State, Status_Code) |>
      mutate(Status_Code = as.character(Status_Code)) |>
      mutate(Status_Code = ifelse(Status_Code == "No Data", NA, Status_Code)) |>
      mutate(Status_Code = ifelse(Status_Code == "Missing Provision", 0, 1)) |>
      pivot_wider(id_cols = Source_State, names_from = Provision_Number, values_from = Status_Code) |>
      column_to_rownames("Source_State")

    # if (clickData()$x) {
    #   provision_matrix[clickData()$x, clickData()$y] <- 3
    # }
    
    hm <- heatmaply(provision_matrix, Rowv = FALSE, Colv = FALSE, plot_method = "plotly", showticklabels = c(FALSE, TRUE))
    
    ggplotly(hm)
  })
  
  output$gunTotals <- renderPlotly({
    h <- event_data("plotly_hover")
    if (is.null(h)) {
      h <- -1
    } else {
      h <- h$x
    }
    line_data <- density |> 
      arrange(desc(Total_Guns_Recovered)) |>
      mutate(color = if_else(Provision_Number == h, 1, 0))
    
    line <- ggplot() +
      #scale_x_continuous(breaks = seq(from = 1, to = length(matching$Provision_Name), by = 4)) +
      #Make sure if its for entire united states, no scientific notation!
      geom_point(data = line_data, mapping = aes(x = Provision_Number, y = Total_Guns_Recovered, color = color)) +
      theme(legend.position = "none")
    
    line
  })
  
}


shinyApp(ui, server)




