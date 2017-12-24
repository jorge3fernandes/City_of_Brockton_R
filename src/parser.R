library(stringr)
library(testthat)  # For unittesting: test_that

# Parsing the txt files into a dataframe
# using the library stringr and regex to extract the information 
# from the text files and turn it into a dataframe
#
# this function looks at a multiple files, breaks it down by month, day, 
# parses it and then binds everything together to form one dataset

currentWD <- getwd() # first get the current path just in case
crawlerResultPath <- '../crawler_result_Conversion/'


# List of RegEx used
listOfTxtFiles = list.files(crawlerResultPath, pattern = ".txt")
dayStartRegEx <- "^For Date:\\s*(\\d{2}\\/\\d{2}\\/\\d{4})\\s*-\\s*(\\w*)"
callRegEx <- "^(\\s|\\d{2}-\\d*)\\s*(\\d{4})\\s*(.*?)\\s{2,}(.*)"
locationRegEx <- "^\\s*Location/Address:\\s*(.*)"
officeRegEx <- "^\\s*ID:\\s*(.*)"
callTakerRegEx <- "^\\s*Call Taker:\\s*(.*)"
dispClrdRegEx <- "^\\s*Disp-(\\d{2}:\\d{2}:\\d{2})\\s*(Arvd-\\d{2}:\\d{2}:\\d{2})?\\s*Clrd-(\\d{2}:\\d{2}:\\d{2})"
wasArrestMadeRegEx <- "^\\s*Refer To Arrest:\\s*(.*)"
referToSummonRegEx <- "^\\s*Refer To Summons:\\s*(.*)$"
summonRegEx <- "^\\s*Summons:\\s*(.*)$"
arrestRegEx <- "^\\s*Arrest:\\s*(.*)$"
addressRegEx <- "^\\s*Address:\\s*(.*)$"
ageRegEx <- "^\\s*Arrest:\\s*(.*)$"
chargesRegEx <- "^\\s*Charges:\\s*(.*)$"

parseTxtToDf <- function(filePath){
  
  # Read the file
  text <- readLines(filePath)
  
  # Create an empty DataFrame
  colNamesList <- c("Time", "Call Reason", "Action", "Address", "Officier", 
                    "Call Taker", "Refer To Arrest", "Disp", "Clrd", "Date", "Day",
                    "Refer To Sumons", "Arvd", "Charges", "Summons")
  df <- data.frame(matrix(ncol=length(colNamesList), nrow=0))
  colnames(df) <- colNamesList
  
  foundACall = FALSE
  callTime <- NA
  callReason <- NA
  callAction <- NA
  dateOfCall <- NA
  dayOfCall <- NA
  
  for (i in seq_along(text)) { 
    if (str_detect(text[i], dayStartRegEx)) { 
      re <- str_match(text[i], dayStartRegEx)
      dateOfCall <- re[2]
      dayOfCall <- re[3]
    }
    
    if (str_detect(text[i],callRegEx)) { 
      re <- str_match(text[i],callRegEx)
      callTime <- re[3]
      callReason <- re[4]
      callAction <- re[5]
      foundACall = TRUE
      next
    }
    
    if (foundACall){
      entryNew <- data.frame(matrix(ncol=length(colNamesList), nrow=1),
                             stringsAsFactors=FALSE)
      colnames(entryNew) <- colNamesList
      
      if (str_detect(text[i], locationRegEx)){
        re <- str_match(text[i], locationRegEx)
        entryNew[, "Address"] <- re[2]
      } else if (str_detect(text[i], officeRegEx)){
        re <- str_match(text[i], officeRegEx)
        entryNew[, "Officier"] <- re[2]
      } else if (str_detect(text[i], callTakerRegEx)){
        re <- str_match(text[i], callTakerRegEx)
        entryNew[, "Call Taker"] <- re[2]
      } else if (str_detect(text[i], dispClrdRegEx)){
        re <- str_match(text[i], dispClrdRegEx)
        entryNew[, "Disp"] <- re[2]
        entryNew[, "Arvd"] <- re[3]
        entryNew[, "Clrd"] <- re[4]
      } else if (str_detect(text[i], wasArrestMadeRegEx)){
        re <- str_match(text[i], wasArrestMadeRegEx)
        entryNew[, "Refer To Arrest"] <- re[2]
        referToArrest <- re[2]
      } else if (str_detect(text[i], referToSummonRegEx)){
        re <- str_match(text[i], referToSummonRegEx)
        entryNew[, "Refer To Sumons"] <- re[2]
        referToSummons <- re[2]
      } else if (str_detect(text[i], summonRegEx)){
        re <- str_match(text[i], summonRegEx)
        entryNew[, "Summons"] <- re[2]
      } else if (str_detect(text[i], chargesRegEx)){
        # TODO: right now only gets the first charge
        # need to modify to get all the charges
        re <- str_match(text[i], chargesRegEx)
        entryNew[, "Charges"] <- re[2]
      }
      
      if ( (i+1) < length(text) ){
        if (str_detect(text[i+1], callRegEx)){
          entryNew[, "Day"] <- dayOfCall
          entryNew[, "Date"] <- dateOfCall
          entryNew[, "Action"] <- callAction
          entryNew[, "Call Reason"] <- callReason
          entryNew[, "Time"] <- callTime
          df <- rbind(df, entryNew)
          
          # reset everything since this call is done and a new one is starting
          foundACall = FALSE
          callTime <- NA
          callReason <- NA
          callAction <- NA
          dateOfCall <- NA
          dayOfCall <- NA
        }}
    }
  }
  df
}

parseAllTxtInDir <- function(listOfTxtFiles){
  for (e in seq_along(listOfTxtFiles)) {
    filePath <- paste0(crawlerResultPath, listOfTxtFiles[e])
    # From the txt file at filePath, get a DF
    df <- parseTxtToDf(filePath)
    browser()
  }
}

#parseAllTxtInDir(listOfTxtFiles)

# UnitTests
test_that("Test01: verify the parser is working on test file",{
  crawlerResultPath <- '../crawler_result_Conversion/'
  testPath <- paste0(crawlerResultPath, listOfTxtFiles[1])
  df_parser <- parseTxtToDf(testPath)
  df_test <- read.csv(paste0(crawlerResultPath, 'test_df.csv'))
  expect_that(df_parser, function(x){all.equal(x, df_test)})
})