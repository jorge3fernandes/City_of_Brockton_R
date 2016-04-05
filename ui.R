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

## leafletOutput is used at the ui side to display the rendered map.

drop_down <- sort(unique(substr(testdata$call_reason_action, start = 17,stop = 52)))
crime <- sort(as.character(unique(testdata$charges)))


shinyUI(navbarPage("Brockton Police Log", id="nav",
                   
                   tabPanel("Interactive map(Test Data)",
                            div(class="outer",
                                
                                tags$head(
                                  includeCSS("styles.css")
                                ),
                                
                                leafletOutput("mymap", width = "100%", height = "100%"),
                                
                                # Shiny versions prior to 0.11 should use class="modal" instead.
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h2("Apply Filters"),
                                              
                                              dateRangeInput('date1','Choose Begin Date:',
                                                          start = min(as.Date(testdata$Date), na.rm = TRUE),
                                                          end = max(as.Date(testdata$Date), na.rm = TRUE),
                                                          min = min(as.Date(testdata$Date), na.rm = TRUE),
                                                          max = max(as.Date(testdata$Date), na.rm = TRUE),
                                                          format = "mm/dd/yy",
                                                          separator = " - "
                                              ),
                                               sliderInput("time", "Animate by Time if Day:", 
                                                        min = min(hour(testdata$Date)), 
                                                        max = max(hour(testdata$Date)), 
                                                        value = max(hour(testdata$Date)),
                                                        step = 1,
                                                        animate = TRUE),
                                              selectInput("search", 'Search Reason for the Call', c(Choose = '', drop_down), selectize = TRUE),
                                              selectInput("Charges", 'Search for crime', c(Choose ='', crime), selectize = TRUE)
                                              
                                ),
                                
                                tags$div(id ="cite",
                                         'This is a work in progress.',
                                         tags$em('Please ask for permission if you plan on using this app.'),
                                         ' Contact: Jorge Fernandes (jorge3fernandes@gmail.com).'
                                )
                            )
                   ),
                   
                   tabPanel("Data explorer (Full Dataset)",
                            
                            dataTableOutput("Data")
                            
                   )
))


