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
  
  testdata$lat[testdata$lat > 42.12 | testdata$lat < 41.9] <- 1000000 
  testdata$long[testdata$long > -70.9 | testdata$long < -71.10] <- 1000000
  testdata <- subset(testdata, !is.na(Date))
  
  test <- reactive({
      testdata <- subset(testdata, as.Date(Date) >= input$date1[1] & as.Date(Date) <= input$date1[2])
    if(!is.null(input$Charges)){
      if(input$Charges == "All"){
        testdata
      }
      else{
            testdata <- subset(testdata, charges %in% as.character(input$Charges))
          }
    }
    
    
    
    
    # subset(testdata, as.Date(Date) >= input$date1[1] & as.Date(Date) <= input$date1[2]) %>% subset(hour(testdata$Date) >= input$time[1] & hour(testdata$Date) <= input$time[2])
    # 
    # if(!is.null(input$dayweek)){
    #   if(input$dayweek == "All"){
    #     sfo_crime_data <- subset(sfo_crime_data, DayOfWeek %in% all_week_list)
    #   }
    #   else{
    #     sfo_crime_data <- subset(sfo_crime_data, DayOfWeek == as.character(input$dayweek))
    #   }            
    # }
   
    })
  
  # http://www.treselle.com/blog/crime-analysis-with-shiny-r/
  
  output$Data <- renderDataTable({
    
    Full_df
  })
  output$mymap <- renderLeaflet({
    # define the leaflet map object
    leaflet(data = test() ) %>% 
      addTiles() %>% 
      setView(-71.02016, 42.08667, zoom = 13) %>% addMarkers( ~long, ~lat, popup = paste("<b>","Call reason/Action: ","</b>", test()$call_reason_action,"<br>",
                                                              "<b>","Occurrence Address: ","</b>", test()$address, "<br>",
                                                              "<b>","Charges: ","</b>", test()$charges, "<br>",
                                                              "<b>","Summoned:","</b>", test()$Summons, "<br>",
                                                              "<b>","Arrested: ","</b>", test()$Arrested, "<br>",
                                                              "<b>","Arr/Summ Address: ","</b>", test()$Occurrence_location, "<br>",
                                                              "<b>","Age: ","</b>", test()$Age,"<br>",
                                                              "<b>","Date: ","</b>",test()$Date),clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE))
    
  })
  


})