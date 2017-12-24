library(rvest)  # For webscraping
library(stringr)  # For text manipulation
library(testthat)  # For unittesting: test_that

currentWD <- getwd() # first get the current path just in case
crawlerResultPath <- '../crawler_result_Conversion/'
firstPg <- "http://www.brocktonpolice.com/category/police-log/" 

# get a list of pdf (logs) available for downloading
homepageLinks <- read_html(firstPg) %>% 
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.pdf")

DownloadEverythingOnFirstPage <- function(listOfLinks, savePath){
  # Description: 
    # download everything on the first page for each links in the listOfLinks
  # Arguments: 
    # listOfLinks: a vector of links 
    # savePath: path to save the results in

  library(pdftools)  # For PDF to Text Conversion
  # Log always printed on Thrusday (Thru) so regex for this day
  regexDate <- "Dispatch.*Thru: \\d{2}/\\d{2}/\\d{4}"

  for(i in seq(length(listOfLinks))){
    tryCatch({
      print(paste0("Working on file ", i, "/",
                   length(listOfLinks), " on page 1"))
      homepage_links[i] %>%  # find those that end in .pdf
        download.file(paste0("file",i ,".pdf"),
                      mode="wb")  # Apply the download function

      pdf_txt <- pdf_text(paste0("file", i, ".pdf"))
      txt <- str_c(pdf_txt, collapse="\n") # this may not be needed actually
      date <- str_extract(txt, regexDate) %>%
              str_replace_all("/", "_") %>% 
              str_replace_all(" ", "") %>%
              str_replace_all(":", "")
      name_pdf <- paste0(savePath, date, ".pdf")
      name_txt <- paste0(savePath, date, ".txt")
      file.rename(paste0("file", i, ".pdf"),name_pdf)
      write(pdf_txt, name_txt)
    },error = function(e){
    })
  } 
  print("Finished Page 1")
}


# test to see that we have the same number of PDF as TXT files
# this somewhat ensures that all the PDF have been converted to txt
test_that("Test01: Same number of PDF as TXT files",{
          pdf_files <- list.files(crawlerResultPath, pattern=".pdf")
          txt_files <- list.files(crawlerResultPath, pattern=".txt")
          expect_equal(length(pdf_files), length(txt_files))
    })

