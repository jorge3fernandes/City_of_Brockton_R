library(shiny)
library(leaflet)

## renderLeaflet() is used at server side to render the leaflet map 

shinyServer(function(input, output) {
  output$mymap <- renderLeaflet({
    # define the leaflet map object
    leaflet(data = testdata) %>% 
      addTiles() %>% 
      setView(-71.02016, 42.08667, zoom = 13) %>% 
      addMarkers(~ long, ~ lat, popup = ~call_reason_action, clusterOptions = markerClusterOptions())
    
  })
  
})