library(rvest)  # For webscraping
library(stringr)  # For text manipulation
library(testthat)  # For unittesting: test_that

# TODO(Jorge3fFernandes): Use Apply family to get rif of for loops

crawlerResultPath <- "./crawler_result_Conversion" 

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
    html_nodes(".page-numbers") %>%                 # find all links for the pages
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
      str_subset("\\.pdf")      # only keep the ones ending with .pdf
    allLinks <- append(allLinks, pageLinks)
    p <- p + 1
  }
  return(allLinks)
}

# Won't need this portion since the parser can now read the pdfs straight from the url. 
# Will leave it here in case we need to download the pdfs in the future.
DownloadAllLinks <- function(listOfLinks){
  # Description: 
    # download everything on the first page for each links in the listOfLinks
  # Arguments: 
    # listOfLinks: a vector of links 
    # savePath: path to save the results in
    
  # Gets the time period covered in the log
  lengthAllLinks <- length(listOfLinks)
  for (i in seq(lengthAllLinks)) {
    tryCatch({
      print(paste0("Working on link ", i, "/",
                   lengthAllLinks))
      listOfLinks[i] %>%  
        download.file(file.path(crawlerResultPath, # folder where we want to save the PDFs and
                                paste0("file",i ,".pdf"),   
                                fsep = .Platform$file.sep))
          
          
          # paste0(crawlerResultPath,"/","file",i ,".pdf"),
          #             mode = "wb")  # Apply the download function
    },error = function(e){
    })
  } 
}

AllLinks <- GetAllLinks(firstPg,PgPrefix)
#DownloadAllLinks(AllLinks)

# Will need to come up with a new test criteria since we're trying to move away from storing the pdfs.

# test to see that we have the same number of PDF as TXT files
# this somewhat ensures that all the PDF have been converted to txt
test_that("Test01: Same number of PDF as TXT files",{
  pdf_files <- list.files(crawlerResultPath, pattern = ".pdf")
  txt_files <- list.files(crawlerResultPath, pattern = ".txt")
  print("testing")
  expect_equal(length(pdf_files), length(txt_files))
})

