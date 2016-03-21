library(shiny)
library(leaflet)
## leafletOutput is used at the ui side to display the rendered map.

shinyUI(fluidPage(
  leafletOutput("mymap", width = "100%", height = 1200)
  
  
))