  library(plyr)
  library(RCurl)
  library(stringr)
  library(pdftools)
  library(dplyr)
  library(gtools)
  
  
  # By examining the urls, sometimes there is no leading zero before days and months less than ten.  
  # I also noticed that for some days, the months in the URL are different.
  # Some of the links have the four digits year spelled out.
  
  # Here I'm creating all the urls with leading zeros where days and months are less than ten.
  
  
  prefix <- "http://www.brocktonpolice.com/wp-content/uploads/"
  AllDays <- seq.Date(from = as.Date('2015-04-01'), to = Sys.Date(), by = "day")
  #Here I'm creating all the urls with without leading zeros where days and months are less than ten.
  AllDays_NL <- gsub("0", "", format(AllDays, '%m%d%y'))
  AllDays_NL_5 <- gsub("0", "", format(AllDays, '%m%d%y'))
  links1 <- paste0(prefix, format(AllDays, '%Y/%m/%m%d%y'), '.pdf')
  links2 <- paste0(prefix, format(AllDays, '%Y/%m/'), AllDays_NL,'.pdf')
  links2b <- paste(substring(links1, 1, 57), substring(links1, 59), sep = "")
  links3 <- paste0(prefix, format(AllDays, '%Y/%m/%m%d%Y'), '.pdf')
  links4 <- paste0(prefix, format(AllDays + 30, '%Y/%m/'), format(AllDays,"%m%d%y"),'.pdf')
  links5 <- paste0(prefix, format(AllDays + 30 , '%Y/%m/'), AllDays_NL_5,'.pdf')
  links5b <- paste(substring(links4, 1, 57), substring(links4, 59), sep = "")
  links <- as.character(c(links1,links2,links2b,links3, links4, links5,links5b))
  
  
  # Here, we go against the website and download the data.
  
  
  #assiging a new WD to put the PDFs
  folder <- "/Users/legs_jorge/Documents/Data Science Projects/RBrockton/Police_logs"
  filenames <- str_c(format(seq.Date(from = as.Date("2015-04-01"), 
                                     to = Sys.Date(), by = "day"),"%m_%d_%Y"),".pdf")
  setwd(folder)
  
  for (i in seq_along(links)) {
    tryCatch({
              download.file(as.character(links[i]), paste0("file",i,".pdf"), mode = "wb")
      
      },error = function(e){}
    )
    
  }
  
  # renaming the files and extracting texts from pdf
  pdf_list <- list.files(path = folder, pattern = ".pdf")
  for (i in seq_along(pdf_list)) {
              txt_names[i] <- str_replace(pdf_list[i],".pdf",".txt")
              txt <- str_c(pdf_text(pdf_list[i]), collapse = "\n")
              date <- str_extract(txt, "\\d{2}/\\d{2}/\\d{4}")
              name <- paste0(str_replace_all(date,"/","_"),".pdf")
              file.rename(pdf_list[i],name)
  }
  
  #Updating the dataset
  download_update <- function(){
    
    
                              folder <- "/Users/legs_jorge/Documents/Data Science Projects/RBrockton/Police_logs"
                              setwd(folder)
                              data <- read.csv("full_bpd_calls.csv", row.names = NULL)
                              data$Date <- as.POSIXct(data$Date)
                              prefix <- "http://www.brocktonpolice.com/wp-content/uploads/"
                              AllDays <- seq.Date(from = as.Date(max(data$Date)), to = Sys.Date(), by = "day")
                              #Here I'm creating all the urls with without leading zeros where days and months are less than ten.
                              AllDays_NL <- gsub("0", "", format(AllDays, '%m%d%y'))
                              AllDays_NL_5 <- gsub("0", "", format(AllDays, '%m%d%y'))
                              links1 <- paste0(prefix, format(AllDays, '%Y/%m/%m%d%y'), '.pdf')
                              links2 <- paste0(prefix, format(AllDays, '%Y/%m/'), AllDays_NL,'.pdf')
                              links2b <- paste(substring(links1, 1, 57), substring(links1, 59), sep = "")
                              links3 <- paste0(prefix, format(AllDays, '%Y/%m/%m%d%Y'), '.pdf')
                              links4 <- paste0(prefix, format(AllDays + 30, '%Y/%m/'), format(AllDays,"%m%d%y"),'.pdf')
                              links5 <- paste0(prefix, format(AllDays + 30 , '%Y/%m/'), AllDays_NL_5,'.pdf')
                              links5b <- paste(substring(links4, 1, 57), substring(links4, 59), sep = "")
                              
                              links <- as.character(c(links1,links2,links2b,links3, links4, links5,links5b))
                              filenames <- str_c(format(seq.Date(from = as.Date(max(data$Date)), 
                                       to = Sys.Date(), by = "day"),"%m_%d_%Y"),".pdf")
                              pdf_list <- list.files(path = folder, pattern = ".pdf")
                              Missingfilenames <- filenames[which(!(filenames %in% pdf_list))]
                              links_update <- c(links1,links2,links2b,links3, links4, links5,links5b)
                              miss_subset <- links_update[which(links_update$filenames %in% Missingfilenames),]
                              
                              for (i in seq_along(links_update)) {
                                    tryCatch({
                                      download.file(as.character(links_update[i]), paste0("file",i,".pdf"), mode = "wb")
                                      
                                    },error = function(e){}
                                    )
                                    
                                  
                              }
                              
                              
                              ##### Deleting the empty PDFs#####
                              PDFs_Empty <- list.files(pattern = "*.pdf")   
                              
                              # Use file.size() immediate, instead of file.info(docs)$size:
                              inds <- file.size(PDFs_Empty) < 0.01 
                              
                              # Remove all documents with file.size < 0.01 from the directory
                              file.remove(PDFs_Empty[inds])
                              ######----------------------########
                              #Getting the list of PDFs that haven't been converted yet
                              full_existing_txt <- list.files(path = folder, pattern = ".txt")
                              #get a list of all the PDFs and convert to .txt
                              txt_list <- list.files(path = folder, pattern = ".pdf") %>% str_replace(".pdf",".txt")
                              #extract the missing ones and replace the extension back to pdf 
                              miss_txt_list <- txt_list[which(!(txt_list %in% full_existing_txt))] %>% str_replace(".txt",".pdf")
                              
                              ###### Early Check to see if there is anything new ######
                             
                              if (length(miss_txt_list) == 0 ){
                               
                                stop("No updates today, Jorge! Let's check back tomorrow! It's time to update this man.")
                              }
                              ########################################################
                              #extracting data from new PDFs
                              for (i in seq_along(miss_txt_list)) {
                                
                                txt <- str_c(pdf_text(miss_txt_list[i]), collapse = "\n")
                                date <- str_extract(txt, "\\d{2}/\\d{2}/\\d{4}")
                                name <- paste0(str_replace_all(date,"/","_"),".pdf")
                                file.rename(miss_txt_list[i],name)
  
                              }
                               
                              pdf_list <- list.files(path = folder, pattern = ".pdf")
                              txt_list <- list.files(path = folder, pattern = ".txt") %>% str_replace(".txt",".pdf")
                              miss_txt_list <- pdf_list[which(!(pdf_list %in% txt_list))] 
                              for (i in seq_along(miss_txt_list)) {
                              
                                write(pdf_text(miss_txt_list[i]), file = str_replace(miss_txt_list[i],".pdf",".txt"))
                                print(paste("Working on file: ",miss_txt_list[i]))
                                
                              } 
                              
  
                              ########################## Creating the dataframe ##############################
                              
                              # using the library stringr and regex here we extract the information 
                              # from the text files and turn it into a dataframe
                              new_txt <- str_replace(miss_txt_list,".pdf",".txt") 
                df <- function(new_txt) {
                                text <- readLines(new_txt)
                                
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
                                Time <- str_trim(str_extract(txtparts, "                [[:digit:]]{4}")) 
                                Time <- sub("(..)$", ":\\1", Time) 
                                
                                date <- rep(str_extract(txtparts[1], "\\d{2}/\\d{2}/\\d{4}"),length(Time))
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
                                # BPD_log$Date <- as.POSIXct(BPD_log$Date,format ="%m/%d/%Y %H:%M", tz = "EST")
                                # BPD_log$Month <- month(BPD_log$Date)
                                # BPD_log$Day <- day(BPD_log$Date)
                                
                                return(BPD_log)
                              }
                              # Combining daily logs into one data frame
                              
                              Full_df <- df(new_txt[1])
                              i <- 2
                              while (i < length(new_txt)) {
                                
                                Full_df <- rbind(Full_df,df(new_txt[i])) 
                                
                                i = i + 1
                              } 
                              
                              Full_df$Date <- as.POSIXct(Full_df$Date,format = "%m/%d/%Y %H:%M", tz = "EST")
                              
                              new_data <- smartbind(data,Full_df)
                              new_data <- new_data[!duplicated(new_data),]
                              write.csv(new_data, "full_bpd_calls.csv", row.names = FALSE)
    }
  # use the function to update the dataset
  download_update()
  
  
  
  
  ########################## Creating the dataframe ##############################
  
  # using the library stringr and regex here we extract the information 
  # from the text files and turn it into a dataframe
  new_txt <- str_replace(miss_txt_list,".pdf",".txt") 
  #new_txt <- list.files(path = folder, pattern = ".txt")
  df <- function(new_txt) {
    text <- readLines(new_txt)
    
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
    Time <- str_trim(str_extract(txtparts, "                [[:digit:]]{4}")) 
    Time <- sub("(..)$", ":\\1", Time) 
    
    date <- rep(str_extract(txtparts[1], "\\d{2}/\\d{2}/\\d{4}"),length(Time))
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
    
    Refer_To_Arrest <- str_extract(txtparts, "Refer To Arrest:.*\n") %>% str_replace_all("Refer To Arrest:","") 
    
    Refer_To_Summons <- str_extract_all(txtparts, "Refer To Summons:.*\n")
    Refer_To_Summons <- str_replace_all(Refer_To_Summons, "Refer To Summons:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    
    Summons <- str_extract_all(txtparts, "         Summons:    .*\n") %>% str_replace_all("Summons:","")  
    Summons <- str_replace_all(Summons, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    
    Arrested <- str_extract_all(txtparts, "          Arrest:    .*\n") 
    Arrested <- str_replace_all(Arrested,"Arrest:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    
    Age <- str_extract_all(txtparts,"Age:.*\n") %>% str_replace_all("Age:","")
    Age <- str_replace_all(Age, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    #age_m <- unlist(str_split(Age, ","))
    
    Suspect_Address <- str_extract_all(txtparts,"         Address:    .*\n") %>% str_replace_all("Address:","") %>% str_replace("character\\(0\\)","")
    Occurrence_location <- str_replace_all(Occurrence_location, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    
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
    # BPD_log$Date <- as.POSIXct(BPD_log$Date,format ="%m/%d/%Y %H:%M", tz = "EST")
    # BPD_log$Month <- month(BPD_log$Date)
    # BPD_log$Day <- day(BPD_log$Date)
    
    return(BPD_log)
  }
  # Combining daily logs into one data frame
  
  Full_df <- df(new_txt[1])
  i <- 2
  while (i < length(new_txt)) {
    
    Full_df <- rbind(Full_df,df(new_txt[i])) 
    
    i = i + 1
  } 
  
  Full_df$Date <- as.POSIXct(Full_df$Date,format = "%m/%d/%Y %H:%M", tz = "EST")
  
  
  write.csv(Full_df, "full_bpd_calls.csv", row.names = FALSE)
  
  links_update[3,2]
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
