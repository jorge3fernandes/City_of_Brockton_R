library(RCurl)
library(RJSONIO)
library(plyr)
library(dplyr)
library(stringr)


gg_address <- unique(fnl_data$address_Geo) %>% unlist()
gg_address <- gg_address[gg_address != 'character(0), BROCKTON, MA']
address <- str_replace_all(gg_address,'@',",&") # for LocationIQ

#Using google API(More Accurate than loaction IQ)
#Location IQ will be used as a back-up

url <- function(address, return.call = "json", sensor = "false", API = "") {
  root <- "https://maps.googleapis.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&key=", API, sep = "")
  return(URLencode(u))
}


geoCode <- function(address,verbose=TRUE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status == "OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$types[[1]]
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA, NA))
  }
}


gg_address  <- ldply(gg_address, function(x) geoCode(x))
names(gg_address)  <- c("lat","lon","location_type", "formatted")

gg_address$Actual_Address <- address

setwd("/Users/legs_jorge/Documents/Data Science Projects/RBrockton")

write.csv(gg_address,"gg_address.csv", row.names = FALSE)

fnl_data_geocoded = left_join(fnl_data,gg_address, by=c("address_Geo" = "Actual_Address"))


#Using LocationIQ API 
#i.e.https://unwiredlabs.com/v2/search.php?token=YOUR_API_TOKEN&q=SEARCH_STRING
#Documentation https://unwiredlabs.com/docs/?python#locationiq

IQ_url <- function(address, API = "") {
  IQ_root <- "https://unwiredlabs.com/v2/search.php?token="
  IQ_u <- paste(IQ_root, API,"&q=", address,  sep = "")
  return(URLencode(IQ_u))
}


IQ_geoCode <- function(address,verbose=TRUE) {
  if (verbose) cat(address,"\n")
  IQ_u <- IQ_url(address)
  IQ_doc <- getURL(IQ_u)
  IQ_x <- fromJSON(IQ_doc,simplify = FALSE)
  if( IQ_x$status == "ok"){
    IQ_lat <- IQ_x$address[[1]]$lat
    IQ_lng <- IQ_x$address[[1]]$lon
    #IQ_location_type  <- IQ_x[[1]]$type
    IQ_formatted_address  <- IQ_x$address[[1]]$display_name
    #IQ_address_type <- IQ_x$results[[1]]$types[[1]]
    #IQ_address_importance <- IQ_x[[1]]$importance
    return(c(IQ_lat, IQ_lng,IQ_formatted_address))
    Sys.sleep(0.5)
  }else {
    return(c(NA,NA, NA))
  }
  
}

IQ_locations  <- ldply(address[1:100], function(x) IQ_geoCode(x))
names(IQ_locations)  <- c("lat","lon","formatted")
IQ_locations$address <- address[1:100]





