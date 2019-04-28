library(dygraphs)
library(xts)
library(fuzzyjoin)
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
library(DT)
library(plotly)
library(magrittr)

#devtools::install_version("MASS", "7.3-51.1", repos = "http://cran.us.r-project.org")  # Resolve incompatibility with shinyapps.io deployment 


disptch_data <- read.csv("./data/cleanData.csv", stringsAsFactors = FALSE) 
address_dt <- read.csv("./data/gg_address.csv", stringsAsFactors = FALSE)

ent_dt <- left_join(disptch_data,address_dt, by = c("addressGeo" = "Actual_Address"))
rm(disptch_data,address_dt)
ent_dt$timeStamp <- as.POSIXct(ent_dt$timeStamp, format = "%m/%d/%Y %H:%M", tz = "GMT")
ent_dt$Date <- as.Date(ent_dt$Date, format = "%m/%d/%Y")
ent_dt$WeekDays <- weekdays(ent_dt$Date)
ent_dt$lat <- as.numeric(ent_dt$lat)
ent_dt$lon <- as.numeric(ent_dt$lon)

ent_dt <- ent_dt %>%
  filter(year(Date) >= max(year(ent_dt$Date))-1) # Only past 2 years

Arrest_Summon = subset(ent_dt, !is.na(summons)|!is.na(arrest))

drop_down <- sort(unique(substr(ent_dt$callReason, start = 17,stop = 52)))
crime <- c("All", sort(as.character(unique(ent_dt$charges))))