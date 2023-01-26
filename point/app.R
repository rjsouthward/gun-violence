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
#For law comparison heatmap
library(heatmaply)
library(scales)

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

#For inflow data, statistics are related to the states SOURCING the guns.  
inflow_data <- left_join(data, pop_data, by = c('Source_State' = 'State', 'Year' = 'Year')) |>
  mutate(Guns_Recovered_PerCapita = (Guns_Recovered * 100000) / Population)

#Inversely, for outflow data statistics are attributed to the states where the guns END UP in. 
outflow_data <- left_join(data, pop_data, by = c('Recovery_State' = 'State', 'Year' = 'Year')) |>
  mutate(Guns_Recovered_PerCapita = (Guns_Recovered * 100000) / Population)

#Render state geoms
map <- get_urbn_map(map = 'territories_states') |>
  #Filter out states with no available data. 
  filter(! state_name %in% c("American Samoa", "Guam", "Mariana Islands", "Virgin Islands"))

stateList <- state.name
stateList <- append(stateList, 'Puerto Rico')

# Include state adjacency data:
state_adjacency <- read_rds("data/state-adjacency.rds")

state_abbs <- state.abb
state_abbs <- append(state_abbs, "DC", after = 7)

state_names <- state.name
state_names <- append(state_names, "District of Columbia", after = 7)

abb_name_matching <- data.frame(abb = state_abbs, name = state_names)

#Import data for state laws. 
provisions <- read_rds('data/state-provisions-2010-2018.rds') |>
  filter(Current_Status != "Repealed") |>
  select(-Current_Status)

#Add missing description in data
prov_descs <- as.vector(unique(provisions$Provision_Description))
prov_descs <- append(prov_descs, "No additional finding is required before the firearm surrender provisions apply", after = 62)

#Save data frame of provision descriptions and names for matching later. 
matching <- data.frame(Provision_Name = as.vector(unique(provisions$Provision_Name)),
                       Provision_Description = prov_descs)

#Provision data only goes up to 2018, so add junk data for years 2019 and 2020
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

#Fix up NA values
all_provisions <- left_join(all_provisions, provisions, by = c("State_Name" = "State_Name", "Year" = "Year", "Provision_Name" = "Provision_Name")) |>
  #Add laws that states are missing to 'Status_Code'
  mutate(Status_Code = if_else(is.na(Status_Code), "Missing Provision", Status_Code)) |>
  #No data for PR or DOC. 
  mutate(Status_Code = if_else(State_Name %in% c("Puerto Rico", "District of Columbia"), "No Data", Status_Code)) |>
  #Data only goes up until Y2018.
  mutate(Status_Code = if_else(Year %in% 2019:2020, "No Data", Status_Code)) |>
  mutate(Status_Code = as.factor(Status_Code))

provision_data <- left_join(data, all_provisions, by = c("Source_State" = "State_Name", "Year" = "Year"))

#Create dataset from which to construct denisty plot that shows which laws (in absense) source the most guns. 
density <- provision_data |> 
  filter(Status_Code == "Missing Provision") |>
  group_by(Provision_Name) |>
  summarize(Total_Guns_Recovered = sum(Guns_Recovered)) |>
  arrange(desc(Total_Guns_Recovered)) |>
  rowid_to_column("Provision_Number")

#Combine gun data and provision data. 
matching <- left_join(matching, density, by = c('Provision_Name' = 'Provision_Name')) |>
  select(-Total_Guns_Recovered)

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
             title = "Show data table ⌄", 
             dataTableOutput(outputId = 'table'),
             downloadButton("downloadData", "Download"),
             p(""),
             p("*Disclaimers:"),
             p("1. Percentages will not sum to 100% if less than 50 states are returned, and U.S territories are excluded."),
             p("2. Per capita statistics are calculated in terms of the source state."),
             p("3. All Data is sourced from the ATF Gun Summary Statistics.")
           )
    )
  ), 
  fluidRow(
    column(8, plotlyOutput("provisions")),
    column(4, plotlyOutput("gunTotals"))
  )),
  tabPanel("Action", "filler"),
  tabPanel("About", "filler")
  )
)

server <- function(input, output){
  #Reactive data relating to gunflow. 
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
  #Reactive data relating to provisions:
  
  #Which states are adjacent to given source state?
  adjReactive <- reactive({
    adj_data <- left_join(state_adjacency, abb_name_matching, by = c("StateCode" = "abb"))
    adj_states <- left_join(adj_data, abb_name_matching, by = c("NeighborStateCode" ="abb")) |>
      select(3:4) |>
      rename("State" = "name.x", "Neighbor" = "name.y") |>
      filter(State == input$state) |>
      pull(Neighbor)
    adj_states
  })
  
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
  
  output$provisions <- renderPlotly({
    #No data for years 2019 and 2020, display error message
    if (input$year %in% c(2019, 2020)) {
      plot(0, type='n')
      #"Provision hetamap cannot be displayed as there is no data is available for the years 2019 and 2020."
    } else {
      provision_matrix <- provReactive() |>
        select(Provision_Number, Source_State, Status_Code) |>
        mutate(Status_Code = as.character(Status_Code)) |>
        mutate(Status_Code = ifelse(Status_Code == "No Data", NA, Status_Code)) |>
        mutate(Status_Code = ifelse(Status_Code == "Missing Provision", 0, 1)) |>
        pivot_wider(id_cols = Source_State, names_from = Provision_Number, values_from = Status_Code) |>
        column_to_rownames("Source_State") 
      
      #Reorder by provision number. 
      provision_matrix <- provision_matrix |> select(sapply(1:length(colnames(provision_matrix)), function(x) as.character(x)))
      
      #Convert cell values to provision numbers. 
      provision_descriptions <- provision_matrix
      for (row in rownames(provision_descriptions)) {
        provision_descriptions[row,] <- seq(from = 1, to = ncol(provision_descriptions), by = 1)
      }
      provision_descriptions[] <- lapply(provision_descriptions, function(x) matching$Provision_Description[match(x, matching$Provision_Number)])
      
      #See which adjacent states are within the top n of guns sourced and which are not. 
      adj_row_colors <- data.frame(State = rownames(provision_matrix)) |>
        mutate(rc = if_else(State %in% adjReactive(), 'Neighboring State', 'Non-Neighboring State')) |>
        mutate(rc = if_else(State == input$state, 'Recovery State', rc))
      
      hm <- heatmaply(provision_matrix, Rowv = FALSE, Colv = FALSE, plot_method = "plotly", 
                      showticklabels = c(FALSE, TRUE), colors = c('#dedcdc', '#AF251F'), row_side_colors = data.frame(Relationship = adj_row_colors$rc), 
                      hide_colorbar = TRUE, custom_hovertext = provision_descriptions)
      
      hm
    }
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
      mutate(color = if_else(Provision_Number == h, 1, 0)) |>
      mutate(color = as.factor(color))
    
    line <- ggplot() +
      #scale_x_continuous(breaks = seq(from = 1, to = length(matching$Provision_Name), by = 4)) +
      #Make sure if its for entire united states, no scientific notation!
      geom_point(data = line_data, mapping = aes(x = Provision_Number, y = Total_Guns_Recovered, color = color)) +
      scale_color_manual(values = c('#C0C0C0', '#AF251F')) +
      scale_y_continuous(labels = label_number()) +
      theme(legend.position = "none")
    
    line
  })
  
}

shinyApp(ui = ui, server = server)
