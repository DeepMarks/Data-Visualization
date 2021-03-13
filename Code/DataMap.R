# Plotting Economic Developent
# FIRST: set the working directory!
# Type ctrl-L to clear the console


# ATP Tennis Ranking

rm(list = ls())
graphics.off()

library(tidyverse)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(data.table)
library(ggmap)


load("gps.RData")

names(gps)[1]<-paste("long")
names(gps)[2]<-paste("lat")


# Creating animation with shiny
library(shiny)


library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("maptype", "Type of Map",
                            choices = providers, selected = providers$OpenStreetMap
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE),
                sliderInput("track", "Track", min(gps$track_seg_point_id), max(gps$track_seg_point_id),
                            value = max(gps$track_seg_point_id), step = 1, animate = animationOptions(loop=FALSE, interval = 200)
                )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    gps[gps$track_seg_point_id <= input$track,]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, gps$ele, reverse = F)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(gps) %>% addProviderTiles(input$maptype) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10, weight = 1, color = "#777777",
                 fillColor = ~pal(ele), fillOpacity = 1, popup = ~paste(track_seg_point_id)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = gps)
    
    x <- ~ele
  
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = colorNumeric(input$colors, gps$ele, reverse = TRUE), values = x, title = "Elevation", labFormat = labelFormat(suffix="m", transform = function(x) sort(x, decreasing = TRUE))
      )
    }
  })
}

shinyApp(ui, server)
