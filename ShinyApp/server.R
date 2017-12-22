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
library(fuzzyjoin)

setwd("/Users/legs_jorge/Documents/Data Science Projects/RBrockton")

disptch_data <- read.csv("Dispatch.csv", stringsAsFactors = FALSE) 
address_dt <- read.csv("gg_address.csv", stringsAsFactors = FALSE)

ent_dt <- left_join(disptch_data,address_dt, by = c("address_Geo" = "Actual_Address"))
ent_dt$timeStamp <- as.POSIXct(ent_dt$timeStamp, format = "%m/%d/%Y %H:%M", tz = "GMT")
ent_dt$date <- as.Date(ent_dt$date, format = "%m/%d/%Y")
ent_dt$WeekDays <- weekdays(ent_dt$date)

Arrest_Summon = subset(ent_dt, !is.na(Summons)|!is.na(Arrested))

## renderLeaflet() is used at server side to render the leaflet map 
shinyServer(function(input, output) {
  
  ent_dt <- subset(ent_dt, !is.na(date))
  
  test <- reactive({
    ent_dt <- subset(ent_dt, as.Date(date, format = "%m/%d/%Y") >= input$date1[1] & as.Date(date) <= input$date1[2]) %>%
                subset(hour(ent_dt$timeStamp) >= input$time[1] & hour(ent_dt$timeStamp) <= input$time[2])
    if(!is.null(input$Charges)){
      if(input$Charges == "All"){
        ent_dt
      }
      else{
        ent_dt <- subset(ent_dt, charges %in% as.character(input$Charges))
          }
    }
    
        })
  
  # http://www.treselle.com/blog/crime-analysis-with-shiny-r/
  
  output$Data <- renderDataTable({
    
    ent_dt
  })
  output$mymap <- renderLeaflet({
    # define the leaflet map object
    if(input$graph == 'Arrests/Summons'){
      arrest_summons = test() %>% subset(!is.na(Summons)|!is.na(Arrested))
    leaflet(data = arrest_summons) %>% 
      addTiles() %>% 
      setView(-71.02016, 42.08667, zoom = 13) %>% addMarkers( ~lon, ~lat, popup = paste("<b>","Call reason/Action: ","</b>", arrest_summons$call_reason_action,"<br>",
                                                              "<b>","Occurrence Address: ","</b>", arrest_summons$formatted, "<br>",
                                                              "<b>","Charges: ","</b>", arrest_summons$charges, "<br>",
                                                              "<b>","Summoned:","</b>", arrest_summons$Summons, "<br>",
                                                              "<b>","Arrested: ","</b>", arrest_summons$Arrested, "<br>",
                                                              "<b>","Arr/Summ Address: ","</b>", arrest_summons$Suspect_Address, "<br>",
                                                              "<b>","Age: ","</b>", arrest_summons$Age,"<br>",
                                                              "<b>","date: ","</b>",arrest_summons$date),clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE, removeOutsideVisibleBounds = TRUE))}
    else{
      leaflet(data = test() ) %>% 
        addTiles() %>% 
        setView(-71.02016, 42.08667, zoom = 13) %>% addMarkers( ~lon, ~lat, popup = paste("<b>","Call reason/Action: ","</b>", test()$call_reason_action,"<br>",
                                                                                          "<b>","Occurrence Address: ","</b>", test()$formatted, "<br>",
                                                                                          "<b>","Charges: ","</b>", test()$charges, "<br>",
                                                                                          "<b>","Summoned:","</b>", test()$Summons, "<br>",
                                                                                          "<b>","Arrested: ","</b>", test()$Arrested, "<br>",
                                                                                          "<b>","Arr/Summ Address: ","</b>", test()$Suspect_Address, "<br>",
                                                                                          "<b>","Age: ","</b>", test()$Age,"<br>",
                                                                                          "<b>","date: ","</b>",test()$date),clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE, removeOutsideVisibleBounds = TRUE))
    }
    
  })
  
  
colnames(ent_dt)
  
  output$summary <- renderPlotly({
    
      temp <- tally(group_by(ent_dt, hour(timeStamp)))
      colnames(temp) <- c("Time", "Count")
      
    plot_ly(temp, x = temp$Time, y = temp$Count)
  })

  output$summary2 <- renderPlotly({
    
    temp <- tally(group_by(ent_dt, WeekDays))
    colnames(temp) <- c("Weekdays", "Count")
    temp$Weekdays <- factor(temp$Weekdays, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    temp <- arrange(temp, Weekdays)
    plot_ly(temp, x = temp$Weekdays, y = temp$Count)
  })
  
  output$trend <- renderPlotly({
    trend <- tally(group_by(ent_dt, as.Date(date, format = "%y %B")))
    colnames(trend) <- c("date", "Count")
    trend$date <- as.Date(trend$date, "%Y %b")
    plot_ly(trend, x = trend$date, y = trend$Count)
  })
  
  output$table <- renderDataTable({
    
    fnl2 <- ent_dt %>% group_by(Month = as.Date(date, format = "%y %B")) %>%
      summarize(n = n())
  })
    
})











