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

#setwd("./ShinyApp/data")

#disptch_data <- read.csv("Dispatch.csv", stringsAsFactors = FALSE) 
#address_dt <- read.csv("gg_address.csv", stringsAsFactors = FALSE)

disptch_data <- dataTotal_clean
address_dt <- gg_address_view

ent_dt <- left_join(disptch_data,address_dt, by = c("addressGeo" = "Actual_Address"))

ent_dt$timeStamp <- as.POSIXct(ent_dt$timeStamp, format = "%m/%d/%Y %H:%M", tz = "GMT")
ent_dt$Date <- as.Date(ent_dt$Date, format = "%m/%d/%Y")
ent_dt$WeekDays <- weekdays(ent_dt$Date)
ent_dt$lat <- as.numeric(ent_dt$lat)
ent_dt$lon <- as.numeric(ent_dt$lon)

Arrest_Summon = subset(ent_dt, !is.na(summons)|!is.na(arrest))

## renderLeaflet() is used at server side to render the leaflet map 
shinyServer(function(input, output) {
  
  ent_dt <- subset(ent_dt, !is.na(Date))
  
  test <- reactive({
    ent_dt <- subset(ent_dt, as.Date(Date, format = "%m/%d/%Y") >= input$Date1[1] & as.Date(Date) <= input$Date1[2]) %>%
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
      arrest_summons = test() %>% subset(!is.na(summons)|!is.na(arrest))
    leaflet(data = arrest_summons) %>% 
      addTiles() %>% 
      setView(-71.02016, 42.08667, zoom = 13) %>% addMarkers( ~lon, ~lat, popup = paste("<b>","Call reason/Action: ","</b>", arrest_summons$call_reason_action,"<br>",
                                                              "<b>","Occurrence Address: ","</b>", arrest_summons$formatted, "<br>",
                                                              "<b>","Charges: ","</b>", arrest_summons$charges, "<br>",
                                                              "<b>","Summoned:","</b>", arrest_summons$summons, "<br>",
                                                              "<b>","Arrested: ","</b>", arrest_summons$arrest, "<br>",
                                                              "<b>","Arr/Summ Address: ","</b>", arrest_summons$Suspect_Address, "<br>",
                                                              "<b>","Age: ","</b>", arrest_summons$Age,"<br>",
                                                              "<b>","Date: ","</b>",arrest_summons$Date),clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE, removeOutsideVisibleBounds = TRUE)) %>%
                                                  addEasyButton(easyButton(
                                                    icon="fa-crosshairs", title="Locate Me",
                                                    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))}
    else{
      leaflet(data = test() ) %>% 
        addTiles() %>% 
        setView(-71.02016, 42.08667, zoom = 13) %>% addMarkers( ~lon, ~lat, popup = paste("<b>","Call reason/Action: ","</b>", test()$call_reason_action,"<br>",
                                                                                          "<b>","Occurrence Address: ","</b>", test()$formatted, "<br>",
                                                                                          "<b>","Charges: ","</b>", test()$charges, "<br>",
                                                                                          "<b>","Summoned:","</b>", test()$summons, "<br>",
                                                                                          "<b>","Arrested: ","</b>", test()$arrest, "<br>",
                                                                                          "<b>","Arr/Summ Address: ","</b>", test()$Suspect_Address, "<br>",
                                                                                          "<b>","Age: ","</b>", test()$Age,"<br>",
                                                                                          "<b>","Date: ","</b>",test()$Date),clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE, removeOutsideVisibleBounds = TRUE)) %>%
                                                addEasyButton(easyButton(
                                                  icon="fa-crosshairs", title="Locate Me",
                                                  onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
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
    trend <- tally(group_by(ent_dt, as.Date(Date, format = "%y %B")))
    colnames(trend) <- c("Date", "Count")
    trend$Date <- as.Date(trend$Date, "%Y %b")
    plot_ly(trend, x = trend$Date, y = trend$Count)
  })
  
  output$table <- renderDataTable({
    
    fnl2 <- ent_dt %>% group_by(Month = as.Date(Date, format = "%y %B")) %>%
      summarize(n = n())
  })
    
})











