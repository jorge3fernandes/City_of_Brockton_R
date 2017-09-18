library(RCurl)
library(RJSONIO)
library(plyr)
library(svDialogs)


address <- unique(fnl_data$address_Geo) %>% unlist()
address <- address[address!= 'character(0), BROCKTON, MA']
intersection <- address['&' == str_extract(address,'&')] # identifying all intersections
intersection <- intersection[!is.na(intersection)]
#address <- address[address != intersection] #identifying normal addresses





#Using google API
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
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA, NA))
  }
}


locations  <- ldply(intersection[1:100], function(x) geoCode(x))
names(locations)  <- c("lat","lon","location_type", "formatted")
head(locations)

locations$test <- intersection[1:100]


#Using LocationIQ API 
#i.e.http://locationiq.org/v1/search.php?key=<API_KEY>&format=json&q=Empire%20State%20Building

IQ_url <- function(address, return.call = "json", API = "") {
  IQ_root <- "https://locationiq.org/v1/search.php?key="
  IQ_u <- paste(IQ_root, API,"&format=", return.call, "&q=", address,  sep = "")
  return(URLencode(IQ_u))
}


IQ_geoCode <- function(address,verbose=TRUE) {
  if (verbose) cat(address,"\n")
  IQ_u <- IQ_url(address)
  IQ_doc <- getURL(IQ_u)
  IQ_x <- fromJSON(IQ_doc,simplify = FALSE)
  if(x$status == "ok") {
    IQ_lat <- IQ_x[[1]]$lat
    IQ_lng <- IQ_x[[1]]$lon
    IQ_location_type  <- IQ_x[[1]]$type
    IQ_formatted_address  <- IQ_x[[1]]$display_name
    #IQ_address_type <- IQ_x$results[[1]]$types[[1]]
    #IQ_address_importance <- IQ_x[[1]]$importance
    return(c(IQ_lat, IQ_lng, IQ_location_type, IQ_formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA, NA))
  }
}

IQ_locations  <- ldply(address[1:8000], function(x) IQ_geoCode(x))
names(IQ_locations)  <- c("lat","lon","location_type", "formatted")
IQ_locations$address <- address[1:8000]
head(IQ_locations)
