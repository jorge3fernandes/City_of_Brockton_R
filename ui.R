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


disptch_data <- read.csv("Dispatch.csv", stringsAsFactors = FALSE) %>% select(-X)

address_dt <- read.csv("gg_address.csv", stringsAsFactors = FALSE)

ent_dt <- left_join(disptch_data,address_dt, by = c("address_Geo" = "Actual_Address"))
ent_dt$timeStamp <- as.POSIXct(ent_dt$timeStamp, format = "%m/%d/%Y %H:%M")
ent_dt$date <- as.Date(ent_dt$date, format = "%m/%d/%Y")
ent_dt$WeekDays <- weekdays(ent_dt$date)

## leafletOutput is used at the ui side to display the rendered map.

drop_down <- sort(unique(substr(ent_dt$call_reason_action, start = 17,stop = 52)))
crime <- c("All", sort(as.character(unique(ent_dt$charges))))


shinyUI(navbarPage("Brockton Police Log", id = "nav",
                   
                   tabPanel("Interactive map",
                            div(class = "outer",
                                
                                tags$head(
                                  includeCSS("styles.css")
                                 ),
                                
                                leafletOutput("mymap", width = "100%", height = "100%"),
                                
                                # Shiny versions prior to 0.11 should use class="modal" instead.
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = 20, right = "auto", bottom = 60,
                                              width = 330, height = "auto",
                                              
                                              h2("Apply Filters"),
                                              
                                              dateRangeInput('date1','Choose Begin date:',
                                                          start = min(as.Date(ent_dt$date), na.rm = TRUE),
                                                          end = max(as.Date(ent_dt$date), na.rm = TRUE),
                                                          min = min(as.Date(ent_dt$date), na.rm = TRUE),
                                                          max = max(as.Date(ent_dt$date), na.rm = TRUE),
                                                          format = "mm/dd/yy",
                                                          separator = " - "
                                              ),
                                               sliderInput("time", "Animate by Time if Day:", 
                                                        min = 0, 
                                                        max = 23, 
                                                        value = c(0,23),
                                                        step = 1,
                                                        animate = TRUE),
                                              selectizeInput("Charges", 'Search for crime', crime, selected = "All", multiple = TRUE,
                                                             options = NULL),
                                              radioButtons("graph","Map Type:", c("Arrests/Summons","All")),
                                              h5("Call Frequency by Time of Day"),
                                              plotlyOutput("summary", height = 200),
                                              h5("Call Frequency by Weekdays"),
                                              plotlyOutput("summary2",height = 200)         
                                                       
                                              
                                ),
                                
                                tags$div(id = "cite",
                                         'This is a work in progress.',
                                         tags$em('Please ask for permission if you plan on using this app.')
                                )
                            )
                   ),
                   
                   tabPanel("Raw Data",
                            
                            dataTableOutput("Data")
                            
                   )
                   # ,
                   # tabPanel("Summary",
                   #          
                   #         plotlyOutput("trend"),
                   #         
                   #         dataTableOutput("table")
                   #   
                   #   
                   # )
                   
                   
))
