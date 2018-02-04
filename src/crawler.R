library(rvest)  # For webscraping
library(stringr)  # For text manipulation
library(testthat)  # For unittesting: test_that
library(here) # For relative working directory. Looks for where there is a file with .Rproj or .here extension and sets that folder as the root directory. 
library(pdftools)  # For PDF to Text Conversion

# The root directory will automatically be detected if .Rproj or .here file in detect

crawlerResultPath <- '../crawler_result_Conversion';
crawlerResultPath <- file.path(getwd(),                       # gets the working root directory  
                               'crawler_result_Conversion',   # folder where we want to save the PDFs and 
                               fsep = .Platform$file.sep)     # makes sure the path is platform agnostic
# This is a poor practice. Start using absolute path.
# You're using Windows, and Travis is on a UNIX system. So to be safe, just use
# absolute paths!!!
setwd(crawlerResultPath) # ensure the files are being downloaded to the right folder

firstPg <- "http://www.brocktonpolice.com/category/police-log/" 
PgPrefix <- "http://www.brocktonpolice.com/category/police-log/page/" # base url to add page numbers

GetAllLinks <- function(DispLogHomePg, AddtnlPgPrefix){
  # Gather all the links to the PDF dispatch log
  # Args: 
  # DispLogHomePg: The first page containing the links to the PDFs 
  # AddtnlPgPrefix: A prefix to the subsequent pages. i.e. http://www.brocktonpolice.com/category/police-log/page/ 
  # Returns:
  # A list of links
  
  # get a list of pdf (logs) available for downloading
  DispLogHomePg <- read_html(firstPg) %>% 
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     # get the url
    str_subset("\\.pdf")
  # getting the number of pages in the website to cycle through
  pageNum <- read_html(firstPg) %>% 
    html_nodes(".page-numbers") %>%                    # find all links for the pages
    html_attr("href") %>%                           # extracts the urls
    .[!is.na(.)] %>% 
    str_extract_all("page/[[:digit:]]{1}") %>%
    str_replace_all("page/","") %>%
    max(unique(.))
  p <- 2 # start on page 2 since we already scraped the first page
  allLinks <- DispLogHomePg # initializing the list with the homepage links
  
  while (p <= pageNum) {
    url <- paste0(AddtnlPgPrefix,p)
    pageLinks <- read_html(url) %>% 
      html_nodes("a") %>%       # find all nodes on the page
      html_attr("href") %>%     # get the urls
      str_subset("\\.pdf") 
    allLinks <- append(allLinks, pageLinks)
    p <- p + 1
  }
  return(allLinks)
}

DownloadAllLinks <- function(listOfLinks){
  # Description: 
    # download everything on the first page for each links in the listOfLinks
  # Arguments: 
    # listOfLinks: a vector of links 
    # savePath: path to save the results in
    
  # Gets the time period covered in the log
  regexDate <- "Dispatch.*Thru: \\d{2}/\\d{2}/\\d{4}"
  lengthAllLinks <- length(listOfLinks)
  # TODO(Jorge3fFernandes): Use Apply family to get rif of for loops
  for (i in seq(lengthAllLinks)) {
    tryCatch({
      print(paste0("Working on link ", i, "/",
                   lengthAllLinks))
      listOfLinks[i] %>%  
        download.file(paste0("file",i ,".pdf"),
                      mode = "wb")  # Apply the download function

      pdf_txt <- pdf_text(paste0("file", i, ".pdf"))
      txt <- str_c(pdf_txt, collapse = "\n") # this may not be needed actually
      date <- str_extract(txt, regexDate) %>%
              str_replace_all("/", "_") %>% 
              str_replace_all(" ", "") %>%
              str_replace_all(":", "")
      name_pdf <- paste0(date, ".pdf")
      name_txt <- paste0(date, ".txt")
      file.rename(paste0("file", i, ".pdf"),name_pdf)
      write(pdf_txt, name_txt)
    },error = function(e){
    })
  } 
}

AllLinks <- GetAllLinks(firstPg,PgPrefix)
#DownloadAllLinks(AllLinks)


# test to see that we have the same number of PDF as TXT files
# this somewhat ensures that all the PDF have been converted to txt
test_that("Test01: Same number of PDF as TXT files",{
          pdf_files <- list.files(crawlerResultPath, pattern = ".pdf")
          txt_files <- list.files(crawlerResultPath, pattern = ".txt")
          print("testing")
          expect_equal(length(pdf_files), length(txt_files))
    })


