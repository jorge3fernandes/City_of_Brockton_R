library(rvest)  # For webscraping
library(RCurl)
library(stringr)  # For text manipulation
library(testthat)  # For unittesting: test_that

# Let's open all the urls in page p and extract all the pdfs

PDFurl <- function(pageURL){
  # Goes through all the links in a specific page and opens them and extracts all the urls that end with pdf.
  pageLinks <- read_html(pageURL) %>% 
    html_nodes("a") %>%       # find all nodes on the page
    html_attr("href") %>%     # get the urls
    str_subset("\\.pdf")      # only keep the ones ending with .pdf
}

crawlerResultPath <- "./crawler_result_Conversion" 


firstPg <- "http://www.brocktonpolice.com/category/police-log/" # Landing Page
PgPrefix <- "http://www.brocktonpolice.com/category/police-log/page/" # base url to add page numbers

GetAllLinks <- function(DispLogHomePg, AddtnlPgPrefix) {
  # Gather all the links to the PDF dispatch log
  # Args:
  # DispLogHomePg: The first page containing the links to the PDFs
  # AddtnlPgPrefix: A prefix to the subsequent pages. i.e. http://www.brocktonpolice.com/category/police-log/page/
  # Returns:
  # A list of links
  
  start_time <- Sys.time()
  # get a list of links for the pdfs in the first page
  message("Gathering links from all pages within 'http://www.brocktonpolice.com/category/police-log/'")
  
  HomePgLogs <- read_html(firstPg) %>%  # reading the first page
    html_nodes(".more-link") %>%       # find all nodes on the page (used chrome extension "Gadget Selector")
    html_attr("href")                  # get the urls on page p
  
  DispLogHomePg <- future_lapply(HomePgLogs, PDFurl) %>% unlist()
  
  
  allLinks <- DispLogHomePg # initializing the list with the homepage links
  #TODO: Need to find a way to automatically detect the number of pages(p)
  p <-2:100 # start the counter. This will be the different pages in the website
  url <- paste0(PgPrefix, p) # initializing the url variable
  
  url_on_page <- function(url) {
    # Extracts all the urls in a given website
    tryCatch({
      
      read_html(url) %>%
        html_nodes(".more-link") %>%       # find all nodes on the page
        html_attr("href")                  # get the urls on page p
      
    }, error = function(e) {
      
    })
  }
  all_url <- future_lapply(url, url_on_page) %>% unlist()
  end_time <- Sys.time()
  
  message(end_time - start_time)
  
  start_time <- Sys.time()
  message("Gathering the links to all the PDFs")
  
  pageLinks <- future_lapply(all_url, PDFurl) %>% unlist() # applying the PDFurl Function defined above
  
  allLinks <- append(allLinks, pageLinks)
  end_time <- Sys.time()
  
  message(end_time - start_time)
  
  return(allLinks)
}




######## Won't need this portion since the parser can now read the pdfs straight from the url. #######
#KEEPING FOR FUTURE REFERENCE
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





