library(shiny)
library(leaflet)
## leafletOutput is used at the ui side to display the rendered map.

shinyUI(fluidPage(
  
  titlePanel("Filters"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("range", "Range:",
                  min = 1, max = 1000, value = c(200,500))
    )
  ),
  mainPanel( 
  leafletOutput("mymap", width = "100%", height = 1200) 
  )
))