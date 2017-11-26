library(googleway)
library(ggmap)

folder <- "/Users/legs_jorge/Documents/Data Science Projects/RBrockton/Police_logs"
setwd(folder)
GEO_Data <- read.csv("full_bpd_calls.csv", row.names = NULL)

getGeoData <- function(location){
  location <- gsub(' ','+',location)
  #geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,"&key=", sep =""))
  #
  geo_data <- getURL(paste("https://unwiredlabs.com/v2/search.php?token=&q=",location,sep =""))
  
  raw_data_2 <- fromJSON(geo_data)
  return(raw_data_2)
}

test <- geocode(GEO_Data$address_Geo)




geocode(as.character(GEO_Data$address_Geo), )
i = 1
while (i <= length(GEO_Data$address_Geo)) {
                  test[i] <-  google_geocode(address = as.character(GEO_Data$address_Geo[i]),     
                      key = key, simplify = TRUE)
                  
                  print(paste0('Working on ',i,' out of ', length(GEO_Data$address_Geo) ))
                  
                  i = i + 1
}

test$results$formatted_address
test$results$geometry$location$lat
test$results$geometry$location$lat


GEO_Data$address_Geo <- as.character(GEO_Data$address_Geo)

GEO_Data$address_Geo[is.na(GEO_Data$address_Geo)] <- 0
getGeoDetails <- function(address_Geo){ 
  address_Geo <- as.character(address_Geo) 
  #use the gecode function to query google servers
  geo_reply = google_geocode(address_Geo, key = key)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat = NA, 
                       long = NA, 
                       accuracy = NA, 
                       formatted_address = NA, 
                       address_type=NA, 
                       status=NA)
  answer$status <- geo_reply$status
  
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}
getGeoDetails(GEO_Data$address_Geo)
