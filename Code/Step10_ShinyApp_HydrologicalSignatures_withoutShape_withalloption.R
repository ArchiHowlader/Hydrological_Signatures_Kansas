cat("\014") 
rm(list=ls())
library(dataRetrieval)
library(sf)
library(leaflet)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(base64enc)
library(lubridate)  
library(patchwork)
library(ggplot2)
library(grid)
library(stringr)
library(wql)
library(ggplot2)
library(grid)
library(lubridate)
library(measurements)
library(dplyr)
library(tidyverse)
library(openair)
library(heatwaveR)
library(data.table)
library(ggplot2)
library(grid)
library(lubridate)
library(measurements)
library(dplyr)
library(tidyverse)
library(openair)
library(heatwaveR)
library(data.table)
require(methods)
library(investr)
require(gridExtra)
library(Metrics)
library(scales)
library(purrr)
library(dplyr)
library(lubridate)
library(zoo) 
library(writexl)
library(readxl)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)
library(readxl)
library(htmlwidgets)
file_Path_Variable_I<- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/InputFiles"
file_Path_Variable_O <- "/Users/ahowl/Desktop/KGS Data analysis/Steps_Workflow_Sept17/Output"
Medium_StreamflowData <- read_excel(file.path(file_Path_Variable_O, "HydrologicalSignatures_MediumTerm_StreamflowData_Adjustingseason.xlsx"))

desoto_shapefile_path <- file.path(file_Path_Variable_I, "DeSoto_shp/DeSoto.shp")
watershed_shapefile_path <- file.path(file_Path_Variable_I, "WatershedBoundary_KN_20230113/watershed_bndry.shp")
RiverNetwork_path <- file.path(file_Path_Variable_I, "rivers_ksrb/rivers_ksrb.shp")

desoto_shp <- st_read(desoto_shapefile_path)
watershed_shp <- st_read(watershed_shapefile_path)
RiverNetwork_shp <- st_read(RiverNetwork_path)
watershed_shp <- st_transform(watershed_shp, st_crs(desoto_shp))
RiverNetwork_shp <- st_transform(RiverNetwork_shp, st_crs(desoto_shp))
combined_shp <- st_union(desoto_shp, watershed_shp)
#combined_shp <- st_union(combined_shp, RiverNetwork_shp)
combined_shp_leaflet <- st_as_sf(combined_shp)


selected_columns <- Medium_StreamflowData[, 7:ncol(Medium_StreamflowData)]
column_choices <- names(selected_columns)


ui <- fluidPage(
  titlePanel("Station Map and Time Series Viewer Medium Term"),
  sidebarLayout(
    sidebarPanel(
      h4("Select a Station on the Map"),
      selectInput("variable", "Select Variable to Plot:", 
                  choices = setNames(column_choices, column_choices), 
                  selected = "MeanAnnualQ_MMD")
    ),
    mainPanel(
      leafletOutput("map", height = 400),
      plotOutput("timeseries_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  data <- Medium_StreamflowData
  
  output$map <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~station_lon, ~station_lat,
        popup = ~paste0("Station: ", station_name, "<br>Site No: ", site_no),
        layerId = ~site_no
      )%>% 
      addPolylines(
        data = RiverNetwork_shp,
        color = "blue",
        weight = 2
      ) %>%
      addPolylines(
        data = combined_shp_leaflet,
        color = "red",
        weight = 2
      )
  })
  
  observeEvent(input$map_marker_click, {
    site_no_selected <- input$map_marker_click$id
    
    selected_data <- data %>%
      filter(site_no == site_no_selected)
    
    output$timeseries_plot <- renderPlot({
      ggplot(selected_data, aes(x = Year, y = .data[[input$variable]])) +
        geom_line(size=1) +
        geom_point(color = 'red', size = 5) +
        labs(title = paste(input$variable, "for Station", site_no_selected),
             x = "Year", y = paste(input$variable)) +
        theme_minimal() +
        theme_minimal(base_size = 20) +
        theme(axis.text.x = element_text(size = 14)) +
        theme(axis.text.y = element_text(size = 14))
    })
  })
}

shinyApp(ui = ui, server = server)
