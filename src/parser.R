library(stringr)   # For text manipulation
library(testthat)  # For unittesting: test_that
library(pdftools)  # For PDF to Text Conversion

# TODO(Jorge3fFernandes): Use Apply family to get rif of for loops

crawlerResultPath <- "./crawler_result_Conversion" # text and pdf location

listAllPDFs <- list.files(crawlerResultPath, 
                          pattern = ".pdf", 
                          full.names = TRUE )

oneText <- function(pdfList){
  #reads in a list of PDF files from a folder or the web and transforms them into 
  #one text file where each individual line is an element of the list
  #
  #Args:
  # textList: List of PDF files
  #
  #Returns: Returns one text file broken down by line
  #
  #
  startTime <- proc.time()[3]
  appPages <- NULL
  print("Converting PDFs to texts ...")
  listOfTxtFiles <- lapply(pdfList, pdf_text) # converting PDFs to texts
  print(paste("Convertion DONE! - Time lapsed: ", proc.time()[3] - startTime))
  for (i in seq_along(listOfTxtFiles)) {
    
    page <- listOfTxtFiles[[i]] %>% str_split("[\r\n]") %>% unlist()
    appPages <- append(appPages,page)
    
    print(paste0("Appending file ", i))
  }
  
  return(appPages)
}

#Can either choose a list of PDFs from a directory or just use the links (AllLinks) from crawler.R.

fullTxt <- oneText(listAllPDFs)

# #file location
# listOfTxtFiles <- list.files(crawlerResultPath, pattern = ".txt", full.names = TRUE)

GetIndivDays <- function(text){
  
  # Breaks the text file into individual days and creates a lists of daily logs.
  #
  # Args:
  #   pdfTxt: a list of text files.
  #
  # Returns:
  #   a vector/list of daily dispatch log.
  
  daySplitRegEx <- "###DaySplit###" # marks where day begins and is used to split the texts
  callSplitRegEx <- "###CallSplit###" # marks where call begins and is used to split the texts
  logDateRegEx <- "For Date:.*[[:digit:]]{4} "
  logtimeOfDayRegEx <- "        [[:digit:]]{4}  "
  startTime <- proc.time()[3]
  
  
  for (i in seq_along(text)) { 
    
    if (str_detect(text[i],logDateRegEx)) { 
      text[i] <- paste0(daySplitRegEx,text[i] ) # flags where the day starts
    }
    
    if (str_detect(text[i],logtimeOfDayRegEx)) { 
      text[i] <- paste0(callSplitRegEx,text[i] ) # flags where the call starts
    }
  }
  
  dayVector <-  str_c(text, collapse = "\n") %>%
    str_split(.,regex(daySplitRegEx)) %>% # uses daystart flag to split into daily logs
    unlist()
  print(paste("DONE! - Time lapsed: ", proc.time()[3] - startTime))
  return(dayVector)
}

allDays <- GetIndivDays(fullTxt)

CallInfoExtractor <- function(dayLog){
  # Reads in a vector of daily logs and extracts individual call info
  #
  # Args:
  #   dayLog: daily log in .txt format
  #
  # Returns:
  #   a data frame
 
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
  
  
  for (i in seq_along(dayLog)) {
    
    txtparts  <-  unlist(str_split(dayLog[i], callSplitRegEx))
    
    #extracting specific fields
    timeOfDay <- str_extract_all(txtparts,timeOfDayRegEx) %>% 
      sub("(..)$", ":\\1", .)
    logDate <- str_extract(txtparts, dateRegEx)[1] %>%
      rep(length(timeOfDay))
    timeStamp <- paste0(logDate," ",timeOfDay)
    callTaker <- str_extract_all(txtparts, callTakerRegEx) %>% 
      str_replace_all("Call Taker:|\n","") 
    dispAddress <- str_extract_all(txtparts, dispAddressRegEx) %>% 
      str_replace_all("Location/Address:|Vicinity of:|\n","") 
    policeOff <- str_extract_all(txtparts, policeOff1RegEx) %>% 
      str_extract_all(policeOff2RegEx) %>% 
      str_replace_all("ID:    |Patrolman ", "")
    
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
    
    df <- cbind(timeOfDay, logDate, timeStamp, callTaker, dispAddress,
                policeOff, callReasonAction, referToArrest, referToSummon, 
                summons, arrest, age, indivAddress, charges, responseTime ) %>% 
      data.frame(stringsAsFactors = FALSE) 
  }
  
  return(df)  
  
}


cmpltDf <- NULL
daysTotal <- length(allDays)

for (i in seq_along(allDays)) {
  
  df <- CallInfoExtractor(allDays[i])
  
  cmpltDf <- rbind(cmpltDf,df)
  
  print(paste0(i,"/", daysTotal))
}


# Will need to come up with a new test criteria since we're trying to move away from storing the pdfs.

# UnitTests
test_that("Test01: verify the parser is working on test file",{
  testCase <- subset(cmpltDf, as.Date(logDate) == as.Date("01/01/2015"))
  #write.csv(testCase,'./crawler_result_Conversion/test_df.csv', row.names = FALSE) #Used to create a test case
  df_test <- read.csv('./crawler_result_Conversion/test_df.csv')
  expect_that(testCase, function(x){all.equal(x, df_test)})
})


