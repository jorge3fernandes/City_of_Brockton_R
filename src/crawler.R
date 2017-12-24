library(rvest)  # For webscraping
library(stringr)  # For text manipulation
library(testthat)  # For unittesting: test_that
library(pdftools)  # For PDF to Text Conversion
library(here) # For relative working directory. Looks for where there is a file with .Rproj or .here extension and sets that folder as the root directory. 

# The root directory will automatically be detected if .Rproj or .here file in detect

crawlerResultPath <- file.path(getwd(),                       #gets the working root directory  
                               'crawler_result_Conversion',   # folder where we want to  save the PDFs and 
                               fsep = .Platform$file.sep)     # makes sure the path is platform agnostic

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

  # Gets the time period covered in the log
  regexDate <- "Dispatch.*Thru: \\d{2}/\\d{2}/\\d{4}"

  for (i in seq(length(listOfLinks))) {
    tryCatch({
      print(paste0("Working on file ", i, "/",
                   length(listOfLinks), " on page 1"))
      homepageLinks[i] %>%  # find those that end in .pdf
        download.file(paste0("file",i ,".pdf"),
                      mode = "wb")  # Apply the download function

      pdf_txt <- pdf_text(paste0("file", i, ".pdf"))
      txt <- str_c(pdf_txt, collapse = "\n") # this may not be needed actually
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

DownloadEverythingOnFirstPage(homepageLinks,crawlerResultPath)

#moving to the next pages 

# getting the number of pages in the website to cycle through
pagenum <- read_html(firstPg) %>% 
  html_nodes(".page-numbers") %>%                    # find all links for the pages
  html_attr("href") %>%                           # extracts the urls
  .[!is.na(.)] %>% 
  str_extract_all("page/[[:digit:]]{1}") %>%
  str_replace_all("page/","") %>%
  max(unique(.))
p = 2
root = "http://www.brocktonpolice.com/category/police-log/page/" #base url to add page numbers
while (p <= pagenum) {
  url = paste0(root,p)
  page_links <- read_html(url) %>% 
    html_nodes("a") %>%       # find all nodes on the page
    html_attr("href") %>%     # get the urls
    str_subset("\\.pdf")      # get all the
  i = 1
  while (i <= length(page_links)) { 
    # find those that end in .pdf
    print(paste0("working on file ", i, "/",length(page_links), " on page ", p))
    tryCatch({
      page_links[i] %>%
        download.file(paste0("file",i,".pdf"), mode = "wb") # Apply the download function
      
      #renaming the files as its being downloaded
      pdf_txt <- pdf_text(paste0("file", i, ".pdf"))
      txt <- str_c(pdf_txt, collapse = "\n") # this may not be needed actually
      date <- str_extract(txt, regexDate) %>%
        str_replace_all("/", "_") %>% 
        str_replace_all(" ", "") %>%
        str_replace_all(":", "")
      name_pdf <- paste0(savePath, date, ".pdf")
      name_txt <- paste0(savePath, date, ".txt")
      file.rename(paste0("file", i, ".pdf"),name_pdf)
      write(pdf_txt, name_txt)
    },error = function(e){}
    )
    i = i + 1
  } 
  print(paste0("Finished Page ", p))
  p = p + 1
}

# test to see that we have the same number of PDF as TXT files
# this somewhat ensures that all the PDF have been converted to txt
test_that("Test01: Same number of PDF as TXT files",{
          pdf_files <- list.files(crawlerResultPath, pattern = ".pdf")
          txt_files <- list.files(crawlerResultPath, pattern = ".txt")
          expect_equal(length(pdf_files), length(txt_files))
    })


