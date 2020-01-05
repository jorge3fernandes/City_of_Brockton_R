pdfToTable <-  function(pdfList){
  # Reads in a list of PDF files from a folder or the web and transforms them into 
  #   one text file where each individual line is an element of the list
  #
  # Args:
  #   textList: List of PDF files
  #
  # Returns: Returns one text file broken down by line
  #
  
 
  tryCatch({
    textFile <- pdf_text(pdfList) # converting PDF to text
    
    pages <- textFile %>% 
      str_split("[\r\n]") %>% # Spliting the line into individual list element so we can later flag for when the day and call started
      unlist()      
    
    text <- future_lapply(pages,DateFlag) %>% # Flagging where each day starts
      unlist() %>% 
      future_lapply(CallFlag) %>% # Flagging where each call starts
      unlist() %>% 
      str_c(collapse = "\n") %>%
      str_split(.,regex(daySplitRegEx)) %>% # uses daystart flag to split into daily logs
      unlist()
    
    txtparts  <-  str_split(text, callSplitRegEx) %>% # Splits a single day into multiple elements(Individual calls)
      unlist()
    
    #dataFinal <- do.call("rbind", future_lapply(txtparts, callParser))
    dataFinal <- rbindlist(future_lapply(txtparts, callParser))
    
    return(dataFinal)
    
  },error = function(e){})
}

#dataframe <- dataTotal

dataCleaner <- function(dataTotal_clean){
  message("Starting the clean-up process")
  start_time <- Sys.time()
  dataTotal_clean[] <- future_lapply(dataTotal_clean, as.character) # 1st let's make sure we set all columns as character vectors
  dataTotal_clean <- fill(dataTotal_clean, Date) %>% # Fill NA rows with their corresponding dates
    filter(Time != 'character(:0)',# deleting rows where Time equals character(:0)
           Date != is.null(Date)) 
  dataTotal_clean[dataTotal_clean == c("character(0)")] <- NA # Replace character(0) with NA accross all columns
  dataTotal_clean[dataTotal_clean == c("character(0), BROCKTON, MA")] <- NA # Replace "character(0), BROCKTON, MA" with NA accross all columns
  dataTotal_clean$timeStamp <- paste0(dataTotal_clean$Date,' ', dataTotal_clean$Time) # create a time stampfield
  dataTotal_clean[] <- as.data.frame(future_sapply(dataTotal_clean, str_replace_all, pattern="c\\(|\\\\n|\"|\\)", replacement="")) %>%  # Using regex to clean unnecessary characters from data
    future_lapply(trimws,which = "both") # Removing all leading and trailing whitespaces from all columns
  dataTotal_clean <- separate(dataTotal_clean, callReasonAction, sep = "    ",  extra = "merge", into = c("callReason", "action"), remove = TRUE, fill = "right") %>%
    unique()
  dataTotal_clean$action <- trimws(dataTotal_clean$action, which = "both")
  
  message('\n', 'removing excess whitespace', '...\n')
  dataTotal_clean <- dataTotal_clean %>% mutate(addressGeo = str_replace_all(addressGeo, '[[:blank:]]', ' '))
  
  message('removing non-alphanumeric characters', '...\n')
  dataTotal_clean <- dataTotal_clean %>%
    mutate(addressGeo = str_replace_all(addressGeo, fixed('\\'), ''),
           addressGeo = str_replace_all(addressGeo, fixed('"'), ''),
           addressGeo = str_replace_all(addressGeo, '[^[:alnum:] ]', ''),
           addressGeo = str_replace_all(addressGeo, '=', '')
    )
  dataTotal_clean <- dataTotal_clean %>%
    mutate(addressGeo = if_else(str_detect(dataTotal_clean$dispAddress, "@"), 
                                tm::removeNumbers(dataTotal_clean$dispAddress) %>% # formatting intersections for better accuracy
                                  str_replace_all(",", "") %>%
                                  str_replace("@  ", "@ ") %>%
                                  trimws(which = "both"), 
                                dataTotal_clean$addressGeo))
  
  
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time)
  message(paste("Done! Took", round(duration[[1]], 2),  units(duration), "to run."))
  write.csv(dataTotal_clean, "./data/cleanData.csv", row.names = FALSE)
  return(dataTotal_clean)
}

#data_clean_test <- dataCleaner(dt)
