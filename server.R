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
  testdata <- subset(testdata, !is.na(Date))
  
  test <- reactive({
      testdata <- subset(testdata, as.Date(Date) >= input$date1[1] & as.Date(Date) <= input$date1[2])%>% subset(hour(testdata$Date) >= input$time[1] & hour(testdata$Date) <= input$time[2])
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
    if (input$radio == "Clusters"){
    # define the leaflet map object
    leaflet(data = test() ) %>% 
      addTiles() %>% 
      setView(-71.02016, 42.08667, zoom = 13) %>% addMarkers( ~long, ~lat, popup = paste("<b>","Call reason/Action: ","</b>", test()$call_reason_action,"<br>",
                                                              "<b>","Occurrence Address: ","</b>", test()$formatted_address, "<br>",
                                                              "<b>","Charges: ","</b>", test()$charges, "<br>",
                                                              "<b>","Summoned:","</b>", test()$Summons, "<br>",
                                                              "<b>","Arrested: ","</b>", test()$Arrested, "<br>",
                                                              "<b>","Arr/Summ Address: ","</b>", test()$Occurrence_location, "<br>",
                                                              "<b>","Age: ","</b>", test()$Age,"<br>",
                                                              "<b>","Date: ","</b>",test()$Date),clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE))
    }else{
      
      leaflet(data = test() ) %>% 
        addTiles() %>% 
        setView(-71.02016, 42.08667, zoom = 13) %>% addMarkers( ~long, ~lat, popup = paste("<b>","Call reason/Action: ","</b>", test()$call_reason_action,"<br>",
                                                                                           "<b>","Occurrence Address: ","</b>", test()$formatted_address, "<br>",
                                                                                           "<b>","Charges: ","</b>", test()$charges, "<br>",
                                                                                           "<b>","Summoned:","</b>", test()$Summons, "<br>",
                                                                                           "<b>","Arrested: ","</b>", test()$Arrested, "<br>",
                                                                                           "<b>","Arr/Summ Address: ","</b>", test()$Occurrence_location, "<br>",
                                                                                           "<b>","Age: ","</b>", test()$Age,"<br>",
                                                                                           "<b>","Date: ","</b>",test()$Date))
    }
    })

 
 
  


})