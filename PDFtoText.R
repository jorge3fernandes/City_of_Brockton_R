
library(dplyr)
library(stringr)
library(ggmap)
library(ggplot2)
library(maps)
library(googleVis)
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


# text <- readLines(filetxt)

#extracting each event
#Here I use a sample PDF where I used pdfbox to extract the text
#parsing text
text <- readLines("testpdf_pdfbox.txt")

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


#Adding  columns derived from the data
lat_Lon <- geocode(address)
Brockton <- get_map(location = c(lon = -71, lat = 43), zoom = 11, maptype = 'roadmap')
ggplot(Brockton, aes(x = long, y = lat)) +
      geom_polygon() +
      coord_map() +
      geom_point(data = lat_Lon, aes(x = lon, y = lat, size = 1), color = "orange")

sites <- gvisMap(BPD_log,locationvar = "address",tipvar = "call_reason_action", 
                 options = list(displayMode = "Markers", mapType = 'normal', colorAxis = "{colors:['red', 'grey']}",
                              useMapTypeControl = TRUE, enableScrollWheel = 'TRUE'))
plot(sites)




