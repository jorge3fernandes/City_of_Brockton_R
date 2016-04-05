library(shiny)
library(leaflet)
library(plyr)
library(RCurl)
library(stringr)
library(pdftools)
library(dplyr)
library(ggmap)
library(ggplot2)
library(maps)
library(googleVis)
library(sp)
library(data.table)
library(plotly)
library(magrittr)
library(rpivotTable)



## renderLeaflet() is used at server side to render the leaflet map 
shinyServer(function(input, output) {
  
  test <- reactive({
    testdata <- subset(testdata, as.Date(Date) >= input$date1[1] & as.Date(Date) <= input$date1[2]) %>%
      subset(hour(testdata$Date) <= input$time) %>% filter(grepl(input$search,call_reason_action , ignore.case = TRUE)) %>% filter(grepl(input$Charges,charges , ignore.case = TRUE))
    
    })
  
 

  
  
  
  output$Data <- renderDataTable({
    
    Full_df
  })
  output$mymap <- renderLeaflet({
    # define the leaflet map object
    leaflet(data = test() ) %>% 
      addTiles() %>% 
      setView(-71.02016, 42.08667, zoom = 13) %>% addMarkers( ~long, ~lat, popup = paste("Call reason/Action: ", test()$call_reason_action,"<br>",
                                                              "Occurrence Address: ", test()$address, "<br>",
                                                              "Charges: ", test()$charges, "<br>",
                                                              "Summoned:", test()$Summons, "<br>",
                                                              "Arrested: ", test()$Arrested, "<br>",
                                                              "Arr/Summ Address: ", test()$Occurrence_location, "<br>",
                                                              "Age: ", test()$Age), clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE))
    
  })
  


})