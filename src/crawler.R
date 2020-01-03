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

GetAllLinks <- function(DispLogHomePg, AddtnlPgPrefix){
  # Gather all the links to the PDF dispatch log
  # Args: 
  # DispLogHomePg: The first page containing the links to the PDFs 
  # AddtnlPgPrefix: A prefix to the subsequent pages. i.e. http://www.brocktonpolice.com/category/police-log/page/ 
  # Returns:
  # A list of links
  
  
  # get a list of links for the pdfs in the first page
  print("Gathering all links on page 1")
  HomePgLogs <-read_html(firstPg) %>%  # reading the first page
    html_nodes(".more-link") %>%       # find all nodes on the page (used chrome extension "Gadget Selector")
    html_attr("href")                  # get the urls on page p
  
  DispLogHomePg <- lapply(HomePgLogs,PDFurl) %>% unlist() 
  

  allLinks <- DispLogHomePg # initializing the list with the homepage links

  p <- 2 # start the counter. This will be the different pages in the website
  url <- paste0(PgPrefix, p) # initializing the url variable
  
  while(url.exists(url) == TRUE){
    print(paste("Gathering all links on page ", p))
    pageP <- read_html(url) %>% 
      html_nodes(".more-link") %>%       # find all nodes on the page
      html_attr("href")                  # get the urls on page p
    
    pageLinks <-  lapply(pageP, PDFurl) %>% unlist() # applying the PDFurl Function defined above
    
    allLinks <- append(allLinks, pageLinks)
    
    p = p + 1
    url <- paste0(PgPrefix,p)
    
    
  }
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





