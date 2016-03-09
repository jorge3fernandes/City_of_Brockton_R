library(pdftools)
library(dplyr)
library(stringr)
library(ggmap)
library(ggplot2)
library(maps)
library(googleVis)
library(sp)
# download pdftotxt from 
# ftp://ftp.foolabs.com/pub/xpdf/xpdfbin-win-3.03.zip
# and extract to program files folder

# here is a pdf for mining
# url <- "http://brocktonpolice.com/wp-content/uploads/2015/01/01202015.pdf"
# dest <- tempfile(fileext = ".pdf")
# download.file(url, dest, mode = "wb")

# set path to pdftotxt.exe and convert pdf to text
# exe <- "C:\\Program Files\\xpdf\\bin32\\pdftotext.exe"
# system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)

# get txt-file name and open it  
# filetxt <- sub(".pdf", ".txt", dest)
# shell.exec(filetxt)

pdf <- pdf_text("021816.pdf")

# text <- readLines(filetxt)

#extracting each event
#Here I use a sample PDF where I used pdfbox to extract the text
#parsing text
text <- readLines(pdf)

#marking where to split

for (i in seq_along(text)) { 
      
     if (str_detect(text[i],'                [[:digit:]]{4}')) { 
           text[i] <- paste0("CALL BEGINS HERE",text[i] )
     }

}


txt <- str_c(text, collapse = "\n")

#split the text by calls
txtparts <- unlist(str_split(txt, "CALL BEGINS HERE"))

#extracting specific fields
time <- str_trim(str_extract(txtparts, "                [[:digit:]]{4}"))
date <- rep(str_extract(txtparts[1], "\\d{2}/\\d{2}/\\d{4}"),length(time))
Call_taker <- str_replace_all(str_extract(txtparts, "Call Taker:.*\n"),"Call Taker:","" ) %>% str_replace_all("\n","")
address <- (str_replace_all(str_extract(txtparts, "Location/Address:.*\n"),"Location/Address:","") 
            %>% str_replace_all("\n","")) %>% str_replace_all("Apt. #.","") %>% paste0( ", Brockton, Massachusetts, United States") %>% str_replace_all("\\[BRO.*\\]","") 
address <- str_replace_all(address,"NA.*States","")

#sometimes we have a cruiser with two police officer, but only one appears after the string "ID:". 
#I had to distinguish between a patrolman who is a call taker and a patrolman who is just a partner.
police_officer <- NULL
for (i in seq_along(txtparts)) {
      
      police_officer[i] <- str_extract_all(txtparts[i], "ID:.*\n")

      if ( identical(str_extract(txtparts[i], "Patrolman.*\n"),str_extract(Call_taker[i], "Patrolman.*\n"))) { 
            
           police_officer[i] <- paste(police_officer[i],str_extract(txtparts[i], "Patrolman.*\n"))
            
      }
      
}

police_officer <- str_replace_all(police_officer,"ID:","") %>% str_replace_all("character.*NA", "")
police_officer <- str_replace_all(police_officer,"\n","")  %>% str_replace_all("\n","")##########
call_reason_action <- str_extract_all(txtparts, "                [[:digit:]]{4}.*\n") %>% str_replace_all("[[:digit:]]{4}","")
Refer_To_Arrest <- str_extract(txtparts, "Refer To Arrest:.*\n") %>% str_replace_all("Refer To Arrest:","")
Person_arrested <- str_extract_all(txtparts, "            Arrest:    .*\n") %>% str_replace_all("Arrest:","")
Age <- str_extract(txtparts,"Age:.*\n") %>% str_replace_all("Age:","")
arrest_location <- str_extract_all(txtparts,"           Address:    .*\n") %>% str_replace_all("Address:","")
charges <- str_extract_all(txtparts,"Charges:    .*\n") %>% str_replace_all("Charges:","")
response_time <- str_extract_all(txtparts,"Arvd.*\n")


#Putting everything together
BPD_log <- cbind(date,time,Call_taker,call_reason_action,address,police_officer,
                   Refer_To_Arrest,Person_arrested,Age,
                   arrest_location,charges,response_time)
BPD_log <- as.data.frame(BPD_log)
BPD_log[BPD_log == "character(0)"] = NA 
BPD_log[BPD_log == ""] = NA 

##################### This will run after we have all the days merged
# Geocoding script for large list of addresses

#define a function that will process googles server responses for us.
getGeoDetails <- function(address){
      address <- as.character(address)
      #use the gecode function to query google servers
      geo_reply = geocode(address, output = 'all', messaging = TRUE, override_limit = TRUE)
      #now extract the bits that we need from the returned list
      answer <- data.frame(lat = NA, long = NA, accuracy = NA, formatted_address = NA, address_type = NA, status = NA)
      answer$status <- geo_reply$status
      
      #if we are over the query limit - want to pause for an hour
      while (geo_reply$status == "OVER_QUERY_LIMIT") {
            print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
            time <- Sys.time()
            print(as.character(time))
            Sys.sleep(60*60)
            geo_reply = geocode(as.character(address), output = 'all', messaging = TRUE, override_limit = TRUE)
            answer$status <- geo_reply$status
      }
      
      #return Na's if we didn't get a match:
      if (geo_reply$status != "OK") {
            return(answer)
      }   
      #else, extract what we need from the Google server reply into a dataframe:
      answer$lat <- geo_reply$results[[1]]$geometry$location$lat
      answer$long <- geo_reply$results[[1]]$geometry$location$lng   
      if (length(geo_reply$results[[1]]$types) > 0) {
            answer$accuracy <- geo_reply$results[[1]]$types[[1]]
      }
      answer$address_type <- paste(geo_reply$results[[1]]$types, collapse = ',')
      answer$formatted_address <- geo_reply$results[[1]]$formatted_address
      
      return(answer)
}

#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
infile <- "input"
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded.rds')
if (file.exists(tempfilename)) {
      print("Found temp file - resuming from index:")
      geocoded <- readRDS(tempfilename)
      startindex <- nrow(geocoded)
      print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (i in seq_along(BPD_log$address)) {
      print(paste("Working on index", i, "of", length(BPD_log$address)))
      #query the google geocoder - this will pause here if we are over the limit.
      result = getGeoDetails(BPD_log$address[i]) 
      print(result$status)     
      result$index <- i
      #append the answer to the results file.
      geocoded <- rbind(geocoded, result)
      #save temporary results as we are going along
      saveRDS(geocoded, tempfilename)
}

Brockton_map <- gvisMap(geocoded, "formatted_address" , "accuracy", 
                     options = list(showTip = FALSE, 
                                  showLine = TRUE, 
                                  enableScrollWheel = TRUE,
                                  mapType = 'terrain', 
                                  useMapTypeControl = TRUE) )
plot(Brockton_map)
