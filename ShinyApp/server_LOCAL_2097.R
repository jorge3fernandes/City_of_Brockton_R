
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
    
    datatable(ent_dt, filter="top", selection="multiple", escape=FALSE#,
              #options = list(searching = FALSE)
              )
    
  })
  output$mymap <- renderLeaflet({
    # define the leaflet map object
    if(input$graph == 'Arrests/Summons'){
      arrest_summons = test() %>% subset(!is.na(summons)|!is.na(arrest))
    leaflet(data = arrest_summons) %>% 
      addTiles() %>% 
      setView(-71.02016, 42.08667, zoom = 13) %>% addMarkers( ~lon, ~lat, popup = paste("<b>","Call reason: ","</b>", arrest_summons$callReason,"<br>",
                                                                                        "<b>","Action Taken: ","</b>", arrest_summons$action,"<br>",
                                                                                        "<b>","Occurrence Address: ","</b>", arrest_summons$formatted, "<br>",
                                                                                        "<b>","Charges: ","</b>", arrest_summons$charges, "<br>",
                                                                                        "<b>","Summoned:","</b>", arrest_summons$summons, "<br>",
                                                                                        "<b>","Arrested: ","</b>", arrest_summons$arrest, "<br>",
                                                                                        "<b>","Arr/Summ Address: ","</b>", arrest_summons$indivAddress, "<br>",
                                                                                        "<b>","Age: ","</b>", arrest_summons$age,"<br>",
                                                                                        "<b>","Timestamp: ","</b>",arrest_summons$timeStamp),
                                                              clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE, removeOutsideVisibleBounds = TRUE)) %>%
                                                  addEasyButton(easyButton(
                                                    icon="fa-crosshairs", title="Locate Me",
                                                    onClick=JS("function(btn, map){ map.locate({setView: true}); }")))}
    else{
      leaflet(data = test() ) %>% 
        addTiles() %>% 
        setView(-71.02016, 42.08667, zoom = 13) %>% addMarkers( ~lon, ~lat, popup = paste("<b>","Call reason: ","</b>", test()$callReason,"<br>",
                                                                                          "<b>","Action Taken: ","</b>", test()$action,"<br>",
                                                                                          "<b>","Occurrence Address: ","</b>", test()$formatted, "<br>",
                                                                                          "<b>","Charges: ","</b>", test()$charges, "<br>",
                                                                                          "<b>","Summoned:","</b>", test()$summons, "<br>",
                                                                                          "<b>","Arrested: ","</b>", test()$arrest, "<br>",
                                                                                          "<b>","Arr/Summ Address: ","</b>", test()$indivAddress, "<br>",
                                                                                          "<b>","Age: ","</b>", test()$age,"<br>",
                                                                                          "<b>","Timestamp: ","</b>",test()$timeStamp),
                                                                clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE, removeOutsideVisibleBounds = TRUE)) %>%
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











