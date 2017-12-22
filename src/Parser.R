library(stringr)
library(pdftools)
library(dplyr)
library(plyr)
library(gtools)
library(ggmap)
setwd("~/Documents/Data Science Projects/RBrockton/crawler_result_Conversion")
  
  ########################## Creating the dataframe ##############################
  
  # using the library stringr and regex to extract the information 
  # from the text files and turn it into a dataframe
  
# this function looks at a multiple files, breaks it down by month, day, 
# parses it and then binds everything together to form one dataset
disp_txt = list.files(pattern = ".txt")
#disp_txt <- disp_txt[20:23]
file_comp = NULL
fnl_data = NULL
new_data = NULL
for (e in seq_along(disp_txt)) {
    
      
            text <- readLines(disp_txt[e])
            
            #marking where to split
            
            for (i in seq_along(text)) { 
              
              if (str_detect(text[i],'For Date:.*[[:digit:]]{4}')) { 
                text[i] <- paste0("DAY BEGINS HERE ",text[i] )
              }
              
              if (str_detect(text[i],'      [[:digit:]]{4}  ')) { 
                text[i] <- paste0("CALL BEGINS HERE",text[i] )
              }
            }
            
            
            txt <- str_c(text, collapse = "\n")
            
            #split the text by days
            IndivDays <- unlist(str_split(txt, "DAY BEGINS HERE "))
            
             
            for (d in seq_along(IndivDays)) {
              print(paste0("Working on file ",e," out of ",length(disp_txt), ", Day", d))
                        #split the text by calls
                  
                        txtparts <- unlist(str_split(IndivDays[d], "CALL BEGINS HERE"))
                        
                        #extracting specific fields
                        Time <- str_trim(str_extract(txtparts, "       [[:digit:]]{4}")) 
                        Time <- sub("(..)$", ":\\1", Time) 
                        
                        date <- str_extract(txtparts, "\\d{2}/\\d{2}/\\d{4}")[1] %>% rep(length(Time))
                        timeStamp <- paste0(date," ",Time)
                        
                        Call_taker <- str_replace_all(str_extract(txtparts, "Call Taker:.*\n"),"Call Taker:","" ) %>% str_replace_all("\n","")
                        
                        
                        
                        
                        Response_address <- str_extract_all(txtparts, "Location/Address:.*\n|Vicinity of:.*\n") %>% str_replace_all("Location/Address:|Vicinity of:","") %>% str_replace_all("\n","")
                        #Since I'm capturing only the raw data, I'll save the next steps for later
                        address_Geo <- str_replace_all(Response_address,"\\[BRO.*\\]","") %>% str_replace_all(" EXT, ","") %>% str_replace_all(" SQ,", "SQUARE") %>% str_replace_all(' @',", BROCKTON, MA @") %>% str_replace_all(' Apt.*','') %>% paste0( ", BROCKTON, MA") %>% str_replace_all('character(0), BROCKTON, MA ',"") %>% trimws() 
                        
                        police_officer <- str_extract_all(txtparts, "(?s)Location\\/Address:[^\n]*\\R(.*)|(?s)Vicinity of:[^\n]*\\R(.*)") %>% str_extract_all("ID:.*\n|Patrolman.*\n")
                        police_officer <- str_replace_all(police_officer,"ID:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
                        
                        call_reason_action <- str_extract_all(txtparts, "       [[:digit:]]{4}.*\n")
                        call_reason_action <- str_replace_all(call_reason_action, "[[:digit:]]{4}","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","")
                        
                        Refer_To_Arrest <- str_extract_all(txtparts, "Refer To Arrest:.*\n") 
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
                        
                        BPD_log <- cbind(timeStamp, date, Time, Call_taker, call_reason_action, Response_address, address_Geo, police_officer, Refer_To_Summons, Summons, Refer_To_Arrest, Arrested,Age,  Suspect_Address, charges, response_time)
                        BPD_log <- data.frame(BPD_log, stringsAsFactors = FALSE)
                        BPD_log[BPD_log == ""] = NA 
                        BPD_log$Response_address[is.na(BPD_log$Response_address)] <- 0
                        BPD_log <- subset(BPD_log, !is.na(Call_taker))
                        BPD_log = unique(BPD_log)
                        #BPD_log$timeStamp <- as.POSIXct(BPD_log$timeStamp,format ="%m/%d/%Y %H:%M", tz = "EST")
                        new_data = rbind(new_data, BPD_log)
                        BPD_log = NULL
                        rm("timeStamp","date", "Call_taker", "call_reason_action", "Response_address", "address_Geo", "police_officer", "Refer_To_Summons", "Summons", "Refer_To_Arrest", "Arrested","Age",  "Suspect_Address", "charges", "response_time")
            
            } 
            
            fnl_data = rbind(fnl_data, new_data)
            fnl_data = unique(fnl_data)
            
            new_data = NULL
    
}

fnl_data$ID <- row.names(fnl_data) #adding an ID field for each call. this will be crucial when we need to create views


#setting a new working directory
setwd("/Users/legs_jorge/Documents/Data Science Projects/RBrockton")

#Check for new address to be added to the geocoded address database

gg_address_view <- read.csv("gg_address.csv", stringsAsFactors = FALSE) #reading in the geocoded addresses

#getting distinct addresses from the recently parsed data
distinct_address <- unique(fnl_data$address_Geo) 

#checking for addresses that haven't been geocoded
new_address <- distinct_address[!(distinct_address %in% gg_address_view$Actual_Address)]

# new_address <-  c('character(0), BROCKTON, MA',distinct_address)
# new_address[new_address =='character(0), BROCKTON, MA'] <- NA

#geocoding the new addresses and adding them to the database
gg_new_address  <- geocode(new_address, output = "more") %>% select(lat,lon,type,address)

names(gg_new_address)  <- c("lat","lon","location_type", "formatted")

gg_new_address$Actual_Address <- new_address

# appending new geocoded addresses to the database

updtd_address = smartbind(gg_address_view, gg_new_address)

write.csv(updtd_address, "gg_address.csv", row.names = FALSE)
  
write.csv(fnl_data,"Dispatch.csv", row.names = FALSE)

# Preparing file for Qlik Sense cloud
# 
Qlik_data <- subset(fnl_data, as.Date(date, format = "%m/%d/%Y") >= as.Date("01/01/2017", format = "%m/%d/%Y"))

write.csv(Qlik_data,"Qlik_data.csv", row.names = FALSE)
  