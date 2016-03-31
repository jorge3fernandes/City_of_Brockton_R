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




shinyUI(navbarPage("Brockton Police Log", id="nav",
                   
                   tabPanel("Interactive map(Sample Data)",
                            div(class="outer",
                                
                                tags$head(
                                  includeCSS("styles.css")
                                ),
                                
                                leafletOutput("mymap", width = "100%", height= 900),
                                
                                # Shiny versions prior to 0.11 should use class="modal" instead.
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h2("Apply Filters"),
                                              
                                              dateInput('date1',
                                                        label = 'Choose Begin Date:',
                                                        value = min(testdata$Date, na.rm = TRUE)
                                              ),
                                              dateInput('date2',
                                                        label = 'Choose End Date:',
                                                        value = max(testdata$Date, na.rm = TRUE)),
                                               sliderInput("time", "Animate by Time if Day:", 
                                                        min = 0, max = 23, value = 12, animate = TRUE),
                                              plotOutput("hist")
                                              
                                ),
                                
                                tags$div(id="cite",
                                         'This is a work in progress.',
                                         tags$em('Please ask for permission if you plan on using this app.'),
                                         ' Contact: Jorge Fernandes (jorge3fernandes@gmail.com).'
                                )
                            )
                   ),
                   
                   tabPanel("Data explorer (Full Dataset)",
                            
                            dataTableOutput("Data")
                            
                   ),
                   tabPanel("Pivot Table",
                            
                            rpivotTableOutput("mypivot")
                            )
))

