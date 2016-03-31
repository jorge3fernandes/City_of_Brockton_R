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
    testdata <- subset(testdata, as.Date(Date) >= input$date1 & as.Date(Date) <= input$date2) %>%
      subset(hour(testdata$Date) <= input$time)
    
    })
  hits_hour <- reactive({
     count(test(), vars = hour(test()$Date))})
  
  

  
  output$Data <- renderDataTable({
    
    test()
  })
  output$mymap <- renderLeaflet({
    # define the leaflet map object
    leaflet(data = test() ) %>% 
      addTiles() %>% 
      setView(-71.02016, 42.08667, zoom = 13) %>% addMarkers( ~long, ~lat, popup = ~call_reason_action)
    
  })
  output$hist <- renderPlot({
    ggplot(data = hits_hour()) + geom_line(aes(x = vars, y =  n)) + ylim(0,max(hits_hour()$n)) +
      labs(x = "Hour",y = "Number of Calls")
  })
  output$mypivot = renderRpivotTable({
    rpivotTable(data = test())
  })


})