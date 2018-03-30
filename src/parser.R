library(stringr)   # For text manipulation
library(testthat)  # For unittesting: test_that
library(pdftools)  # For PDF to Text Conversion
library(ggmap)

# List of RegEx
callSplitRegEx <- "###CallSplit###"
timeOfDayRegEx <- "       [[:digit:]]{4}"
dateRegEx <- "\\d{2}/\\d{2}/\\d{4}"
callTakerRegEx <- "Call Taker:.*\n"
dispAddressRegEx <- "Location/Address:.*\n|Vicinity of:.*\n"
policeOff1RegEx <- "(?s)Location\\/Address:[^\n]*\\R(.*)|(?s)Vicinity of:[^\n]*\\R(.*)"
policeOff2RegEx <- "ID:.*\n|Patrolman.*\n"
callReasonActionRegEx <- "       [[:digit:]]{4}.*\n"
referToArrestRegEx <- "Refer To Arrest:.*\n"
referToSummonRegEx <- "Refer To Summons:.*\n"
arrestRegEx <- "          Arrest:    .*\n"
summonRegEx <- "         Summons:    .*\n"
ageRegEx <- "Age:.*\n"
indivAddressRegEx <- "         Address:    .*\n"
chargesRegEx <- "Charges:    .*\n"
responseTimeRegEx <- "Arvd.*\n"
daySplitRegEx <- "###DaySplit###" # marks where day begins and is used to split the texts
callSplitRegEx <- "###CallSplit###" # marks where call begins and is used to split the texts
logDateRegEx <- "For Date:.*[[:digit:]]{4} "
logtimeOfDayRegEx <- "        [[:digit:]]{4}  "

DateFlag <- function(text){
  
  if (str_detect(text,logDateRegEx)) { 
    text <- paste0(daySplitRegEx,text ) # flags where the day starts
  }
  return(text)
}
CallFlag <- function(text){
  if (str_detect(text,logtimeOfDayRegEx)) { 
    text <- paste0(callSplitRegEx,text ) # flags where the call starts
  }
  return(text)
}
callParser <- function(txtparts){
  # List of RegEx
  callSplitRegEx <- "###CallSplit###"
  timeOfDayRegEx <- "             [[:digit:]]{4}"
  dateRegEx <- "For Date: \\d{2}/\\d{2}/\\d{4}"
  callTakerRegEx <- "Call Taker:.*\n"
  dispAddressRegEx <- "Location/Address:.*\n|Vicinity of:.*\n"
  policeOff1RegEx <- "(?s)Location\\/Address:[^\n]*\\R(.*)|(?s)Vicinity of:[^\n]*\\R(.*)"
  policeOff2RegEx <- "ID:.*\n|Patrolman.*\n"
  callReasonActionRegEx <- "       [[:digit:]]{4}.*\n"
  referToArrestRegEx <- "Refer To Arrest:.*\n"
  referToSummonRegEx <- "Refer To Summons:.*\n"
  arrestRegEx <- "          Arrest:    .*\n"
  summonRegEx <- "         Summons:    .*\n"
  ageRegEx <- "Age:.*\n"
  indivAddressRegEx <- "         Address:    .*\n"
  chargesRegEx <- "Charges:    .*\n"
  responseTimeRegEx <- "Arvd.*\n"
  daySplitRegEx <- "###DaySplit###" # marks where day begins and is used to split the texts
  callSplitRegEx <- "###CallSplit###" # marks where call begins and is used to split the texts
  logDateRegEx <- "For Date:.*[[:digit:]]{4} "
  logtimeOfDayRegEx <- "        [[:digit:]]{4}  "
  timeOfDayRegEx <- "       [[:digit:]]{4}"
  
  
  #extracting specific fields
  timeOfDay <- str_extract_all(txtparts,timeOfDayRegEx) %>% 
    sub("(..)$", ":\\1", .)
  logDate <- str_extract(txtparts, dateRegEx)[1] %>%
    rep(length(txtparts)) %>% str_replace_all("For Date: ","")
  callTaker <- str_extract_all(txtparts, callTakerRegEx) %>% 
    str_replace_all("Call Taker:|\n","") 
  dispAddress <- str_extract_all(txtparts, dispAddressRegEx) %>% 
    str_replace_all("Location/Address:|Vicinity of:|\n|\\[BRO.*\\]|EXT|\\[ABI 1\\]","") 
  addressGeo <- str_replace_all(dispAddress, " Apt.*","") %>% 
                  str_replace_all(' @',", BROCKTON, MA @") %>% 
                  trimws() %>%
                  paste0(", BROCKTON, MA")
  dispAddress <- paste0(dispAddress, ", BROCKTON, MA")
  policeOff <- str_extract_all(txtparts, policeOff1RegEx) %>% 
    str_extract_all(policeOff2RegEx) %>% 
    str_replace_all("ID:|Patrolman |c\\(\\\"    |\\\n\"", "")
  callReasonAction <- str_extract_all(txtparts, callReasonActionRegEx) %>% 
    str_replace_all("[[:digit:]]{4}","")
  referToArrest <- str_extract_all(txtparts, referToArrestRegEx) %>% 
    str_replace_all("Refer To Arrest:","")
  referToSummon <- str_extract_all(txtparts, referToSummonRegEx) %>%
    str_replace_all("Refer To Summons:|\n","") 
  summons <- str_extract_all(txtparts, summonRegEx) %>% 
    str_replace_all("Summons:","") 
  arrest <- str_extract_all(txtparts, arrestRegEx) %>%
    str_replace_all("Arrest:","")
  age <- str_extract_all(txtparts,ageRegEx) %>% 
    str_replace_all("Age:","") 
  indivAddress <- str_extract_all(txtparts,indivAddressRegEx) %>% 
    str_replace_all("Address:","") 
  charges <- str_extract_all(txtparts,chargesRegEx) %>% 
    str_replace_all("Charges:","")
  responseTime <- str_extract_all(txtparts,responseTimeRegEx) %>%
    str_replace_all("c\\(\\\"    |\\\n\"|\"","") 
  
  df <- data.frame(Time = timeOfDay, Date = logDate, operator = callTaker, 
                   dispAddress = dispAddress, addressGeo = addressGeo, officer = policeOff, callReasonAction = callReasonAction, 
                   referToArrest = referToArrest, referToSummon = referToSummon, summons = summons, 
                   arrest = arrest, age = age, indivAddress = indivAddress, charges = charges, responseTime = responseTime )
          
  
  #df$Date <- na.locf(as.character(df$Date))
  return(df)
}

pdfToTable <- function(pdfList){
  #reads in a list of PDF files from a folder or the web and transforms them into 
  #one text file where each individual line is an element of the list
  #
  #Args:
  # textList: List of PDF files
  #
  #Returns: Returns one text file broken down by line
  #
  textFile <- pdf_text(pdfList) # converting PDF to text
  
  pages <- textFile %>% 
    str_split("[\r\n]") %>% # Spliting the line into individual list element so we can later flag for when the day and call started
    unlist() 
  
  text <- lapply(pages,DateFlag) %>% # Flagging where each day starts
    unlist() %>% 
    lapply(CallFlag) %>% # Flagging where each call starts
    unlist() %>% 
    str_c(collapse = "\n") %>%
    str_split(.,regex(daySplitRegEx)) %>% # uses daystart flag to split into daily logs
    unlist()
  
  txtparts  <-  str_split(text, callSplitRegEx) %>% # Splits a single day into multiple elements(Individual calls)
                  unlist()
  
  dataFinal <- do.call("rbind", lapply(txtparts, callParser))
  
  return(dataFinal)
}


