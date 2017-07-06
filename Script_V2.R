library(stringr)
library(pdftools)
library(dplyr)
library(gtools)
setwd("~/Documents/Data Science Projects/RBrockton/crawler_result_Conversion")
  
  ########################## Creating the dataframe ##############################
  
  # using the library stringr and regex to extract the information 
  # from the text files and turn it into a dataframe
  
# this function looks at a multiple files, breaks it down by month, day, 
# parses it and then binds everything together to form one dataset
disp_txt = list.files(pattern = ".txt")
#disp_txt <- disp_txt[1:3]
  df <- function(new_txt) {
  fnl_dt = NULL   
    for (e in seq_along(disp_txt)) {
            text <- readLines(disp_txt[e])
            
            #marking where to split
            
            for (i in seq_along(text)) { 
              
              if (str_detect(text[i],'For Date:.*[[:digit:]]{4}')) { 
                text[i] <- paste0("DAY BEGINS HERE ",text[i] )
              }
              
              if (str_detect(text[i],'                [[:digit:]]{4}')) { 
                text[i] <- paste0("CALL BEGINS HERE",text[i] )
              }
            }
            
            
            txt <- str_c(text, collapse = "\n")
            
            #split the text by days
            IndivDays <- unlist(str_split(txt, "DAY BEGINS HERE "))
            new_data = NULL
            
            for (d in 2:length(IndivDays)) {
                
                        #split the text by calls
                        
                        txtparts <- unlist(str_split(IndivDays[d], "CALL BEGINS HERE"))
                        
                        #extracting specific fields
                        Time <- str_trim(str_extract(txtparts, "                [[:digit:]]{4}")) 
                        Time <- sub("(..)$", ":\\1", Time) 
                        
                        date <- rep(str_extract(txtparts, "\\d{2}/\\d{2}/\\d{4}"),length(Time))
                        Date <- paste0(date," ",Time)
                        
                        Call_taker <- str_replace_all(str_extract(txtparts, "Call Taker:.*\n"),"Call Taker:","" ) %>% str_replace_all("\n","")
                        
                        
                        
                        
                        Response_address <- str_extract(txtparts, "Location/Address:.*\n|Vicinity of:.*\n") %>% str_replace_all("Location/Address:|Vicinity of:","") %>% str_replace_all("\n","") %>% paste0( ", BROCKTON, MA") %>% str_replace_all("\\[BRO.*\\]","") %>% str_replace_all(" EXT, ","") %>% str_replace_all(" SQ,", "SQUARE")
                        #Since I'm capturing only the raw data, I'll save the next steps for later
                        address_Geo <- str_replace_all(Response_address,' Apt\\. .*, ',', BROCKTON, ')
                        address_Geo <- str_replace_all(address_Geo,"NA, BROCKTON, MA","")
                        
                        police_officer <- str_extract_all(txtparts, "(?s)Location\\/Address:[^\n]*\\R(.*)|(?s)Vicinity of:[^\n]*\\R(.*)") %>% str_extract_all("ID:.*\n|Patrolman.*\n")
                        police_officer <- str_replace_all(police_officer,"ID:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
                        
                        call_reason_action <- str_extract_all(txtparts, "                [[:digit:]]{4}.*\n")
                        call_reason_action <- str_replace_all(call_reason_action, "[[:digit:]]{4}","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","")
                        
                        Refer_To_Arrest <- str_extract(txtparts, "Refer To Arrest:.*\n") 
                        Refer_To_Arrest <- str_replace_all(Refer_To_Arrest, "Refer To Arrest:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
                        
                        Refer_To_Summons <- str_extract_all(txtparts, "Refer To Summons:.*\n")
                        Refer_To_Summons <- str_replace_all(Refer_To_Summons, "Refer To Summons:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
                        
                        Summons <- str_extract_all(txtparts, "         Summons:    .*\n") %>% str_replace_all("Summons:","")  
                        Summons <- str_replace_all(Summons, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
                        
                        Arrested <- str_extract_all(txtparts, "          Arrest:    .*\n") 
                        Arrested <- str_replace_all(Arrested,"Arrest:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
                        
                        Age <- str_extract_all(txtparts,"Age:.*\n") %>% str_replace_all("Age:","")
                        Age <- str_replace_all(Age, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
                        #age_m <- unlist(str_split(Age, ","))
                        
                        Suspect_Address <- str_extract_all(txtparts,"         Address:    .*\n") %>% str_replace_all("Address:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
                        #Occurrence_location <- str_replace_all( Occurrence_location, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
                        
                        charges <- unlist(str_extract_all(txtparts,"Charges:    .*\n") %>% str_replace_all("Charges:",""))
                        charges <- str_replace_all(charges, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
                        
                        response_time <- str_extract_all(txtparts,"Arvd.*\n") 
                        response_time <- str_replace_all(response_time, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace_all("c\\(","") %>% str_replace("character\\(0","")
                        
            
                        #Putting everything together
                        
                        BPD_log <- cbind(Date, Call_taker, call_reason_action, Response_address, address_Geo, police_officer, Refer_To_Summons, Summons, Refer_To_Arrest, Arrested,Age,  Suspect_Address, charges, response_time)
                        BPD_log <- data.frame(BPD_log, stringsAsFactors = FALSE)
                        BPD_log[BPD_log == ""] = NA 
                        BPD_log$Response_address[is.na(BPD_log$Response_address)] <- 0
                        BPD_log <- subset(BPD_log, !is.na(Call_taker))
                        
                        new_data = rbind(new_data, BPD_log)
                        
            }
    } 
  fnl_dt <- smartbind(fnl_dt, new_data)  
  }
    ################# Separating arrests into individual rows####
full_data <- df(disp_txt)  
  
  
  
  
  
  
  
  
  
  
  
  
  
    BPD_logt <- BPD_log %>%  mutate(Distin_PO = strsplit(as.character(Arrested), "\", \"")) %>% unnest(Distin_PO)
    
    
    
    
    
    
    
    
    
    # BPD_log$Date <- as.POSIXct(BPD_log$Date,format ="%m/%d/%Y %H:%M", tz = "EST")
    # BPD_log$Month <- month(BPD_log$Date)
    # BPD_log$Day <- day(BPD_log$Date)
    
    
    
    #############################GEOCODING##############################
    BPD_log$address_Geo <- as.character(BPD_log$address_Geo)
    
    BPD_log$address_Geo[is.na(BPD_log$address_Geo)] <- 0
    getGeoDetails <- function(address_Geo){ 
      address_Geo <- as.character(address_Geo) 
      #use the gecode function to query google servers
      geo_reply = geocode(address_Geo, output = 'all', messaging = TRUE, override_limit = TRUE)
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
    
    #initialise a dataframe to hold the results
    geocoded <- data.frame()
    
    
    for (ii in seq_along(BPD_log$address_Geo )){
      print(paste("Working on index", ii, "of", length(BPD_log$address_Geo )))
      #query the google geocoder - this will pause here if we are over the limit.
      result = getGeoDetails(BPD_log$address_Geo[ii]) 
      print(result$status)     
      result$index <- ii
      #append the answer to the results file.
      geocoded <- smartbind(geocoded, result)
    }
    geocoded <- geocoded[!duplicated(geocoded),]
    BPD_log <- cbind(BPD_log, geocoded)
    
    
    ###################################################################
    
  }
  # Combining daily logs into one data frame
  
  Full_df <- df(new_txt[1])
  i <- 2
  while (i <= length(new_txt)) {
    
    Full_df <- smartbind(Full_df,df(new_txt[i])) 
    
    i = i + 1
  } 
  
  Full_df$Date <- as.POSIXct(Full_df$Date,format = "%m/%d/%Y %H:%M", tz = "EST")
  
  new_data <- smartbind(data,Full_df)
  new_data <- new_data[!duplicated(new_data),]
  write.csv(new_data, "Dispatch_2017.csv", row.names = FALSE)
}
# use the function to update the dataset
download_update()




#### This script uses RCurl and RJSONIO to download data from Google's API:
#### Latitude, longitude, location type (see explanation at the end), formatted address
#### Notice ther is a limit of 2,500 calls per day

library(RCurl)
library(RJSONIO)
library(plyr)

key = 'AIzaSyApIDfIEIjrl0e5henBpA-8GWOwSdljAF8'
url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address,"&key=",key,"&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

test_address <- laply(Disp_2017$address_Geo, geoCode())

##Test with a single address
#address <- geoCode("The White House, Washington, DC")
#address
#[1] "38.8976831"
#[2] "-77.0364972"
#[3] "APPROXIMATE"
#[4] "The White House, 1600 Pennsylvania Avenue Northwest, Washington, D.C., DC 20500, USA"

# Use plyr to getgeocoding for a vector
#address <- c("The White House, Washington, DC","The Capitol, Washington, DC")
#locations <- ldply(address, function(x) geoCode(x))
#names(locations) <- c("lat","lon","location_type", "forAddress")

#Location type, for more info check here: https://developers.google.com/maps/documentation/directions/
#"ROOFTOP" indicates that the returned result is a precise geocode for which we have location information accurate down to street address precision.
#RANGE_INTERPOLATED" indicates that the returned result reflects an approximation (usually on a road) interpolated between two precise points (such as intersections). Interpolated results are generally returned when rooftop geocodes are unavailable for a street address.
#GEOMETRIC_CENTER" indicates that the returned result is the geometric center of a result such as a polyline (for example, a street) or polygon (region).
#APPROXIMATE" indicates that the returned result is approximate.


