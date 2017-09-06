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
library(dygraphs)
library(xts)



## renderLeaflet() is used at server side to render the leaflet map 
shinyServer(function(input, output) {
  
  testdata <- subset(testdata, !is.na(Date))
  
  test <- reactive({
    testdata <- subset(testdata, as.Date(Date) >= input$date1[1] & as.Date(Date) <= input$date1[2]) %>%
                subset(hour(testdata$Date) >= input$time[1] & hour(testdata$Date) <= input$time[2])
    if(!is.null(input$Charges)){
      if(input$Charges == "All"){
        testdata
      }
      else{
        testdata <- subset(testdata, charges %in% as.character(input$Charges))
          }
    }
    
        })
  
  # http://www.treselle.com/blog/crime-analysis-with-shiny-r/
  
  output$Data <- renderDataTable({
    
    Full_df
  })
  output$mymap <- renderLeaflet({
    # define the leaflet map object
    if(input$graph == 'Clusters'){
    leaflet(data = test() ) %>% 
      addTiles() %>% 
      setView(-71.02016, 42.08667, zoom = 13) %>% addMarkers( ~long, ~lat, popup = paste("<b>","Call reason/Action: ","</b>", test()$call_reason_action,"<br>",
                                                              "<b>","Occurrence Address: ","</b>", test()$formatted_address, "<br>",
                                                              "<b>","Charges: ","</b>", test()$charges, "<br>",
                                                              "<b>","Summoned:","</b>", test()$Summons, "<br>",
                                                              "<b>","Arrested: ","</b>", test()$Arrested, "<br>",
                                                              "<b>","Arr/Summ Address: ","</b>", test()$Occurrence_location, "<br>",
                                                              "<b>","Age: ","</b>", test()$Age,"<br>",
                                                              "<b>","Date: ","</b>",test()$Date),clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE, removeOutsideVisibleBounds = TRUE))}
    else{
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
  
  

  
  output$summary <- renderPlotly({
    
      temp <- tally(group_by(test(), hour(Date)))
      colnames(temp) <- c("Time", "Count")
      
    plot_ly(temp, x = Time, y = Count)
  })

  output$summary2 <- renderPlotly({
    
    temp <- tally(group_by(test(),weekdays(Date)))
    colnames(temp) <- c("Weekdays", "Count")
    temp$Weekdays <- factor(temp$Weekdays, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    temp <- arrange(temp, Weekdays)
    plot_ly(temp, x = Weekdays, y = Count)
  })
  
  output$trend <- renderPlotly({
    trend <- Full_df %>% group_by(as.Date(Date)) %>% summarize(n=n())
    colnames(trend) <- c("Date", "Count")
    plot_ly(trend, x = Date, y = Count)
  })
  
  output$table <- renderDataTable({
    
    fnl2 <- Full_df %>% group_by(Month = as.Date(Date, format = "%y %B")) %>%
      summarize(n=n())
  })
    
})











