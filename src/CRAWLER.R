library(rvest) # For webscraping
library(stringr) # For text manipulation
library(pdftools) # For PDF to Text Conversion

#set local path
setwd("../crawler_result_Conversion")
############################ PLEASE FIND THE UPDATED CRAWLER AT THE BOTTOM OF THE PAGE ###############################
# root <- read_html("http://www.brocktonpolice.com/category/police-log/")
# 
# ##### Collecting all the links on the side bar ####
# ##### Using Selector Gadget to find the right nodes
# 
# sidebar_links <- root %>% 
#   html_nodes("a") %>% #finds all links on the sidebar
#   html_attr("href") %>% #extracts the urls
#   .[!is.na(.)] # removes NA
# r <- 0 #to generate sequence to the files downloaded
# ##### Following the links to download the PDFs####
# for (i in seq_along(sidebar_links)) {
#   nav <- read_html(sidebar_links[i]) %>% html_nodes(".alignleft") %>% html_attr("href")
#   #after clicking on the sidebar links 
#   for (j in seq_along(nav)) {
#     r <- r + 1
#     tryCatch({
#       read_html(nav[j]) %>%
#         html_nodes("a") %>%       # find all links
#         html_attr("href") %>%     # get the url
#         str_subset("\\.pdf") %>%  # find those that end in pdf
#         
#         download.file(paste0("file",r,".pdf"), mode = "wb") 
#       pdf_txt <- pdf_text(paste0("file",r,".pdf"))
#       txt <- str_c(pdf_txt, collapse = "\n")
#       date <- str_extract(txt, "Dispatch.*Thru: \\d{2}/\\d{2}/\\d{4}")
#       name_pdf <- paste0(str_replace_all(date,"/","_") %>% str_replace_all(" ","") %>% str_replace_all(":",""),".pdf")
#       name_txt <- paste0(str_replace_all(date,"/","_") %>% str_replace_all(" ","") %>% str_replace_all(":",""),".txt")
#       
#       file.rename(paste0("file",r,".pdf"),name_pdf)
#       write(pdf_txt, name_txt)
#     },error = function(e){}
#     )
#     
#   }
# }
# 
# # Deleting PDF's with File name NA
# list.files(pattern = "NA.") %>% file.remove()


                                                                                                ########### Update 11/20/2017 ##############
########### Since the updated the website layout ###############
first_pg = "http://www.brocktonpolice.com/category/police-log/" 

#download everything on the first page 
homepage_links <- read_html(first_pg) %>% 
                    html_nodes("a") %>%       # find all links
                      html_attr("href") %>%     # get the url
                        str_subset("\\.pdf")
i = 1
while (i <= length(homepage_links)) { 
   
    tryCatch({
      print(paste0("working on file ", i, "/",length(homepage_links), " on page 1"))
  homepage_links[i] %>%  # find those that end in .pdf
        download.file(paste0("file",i,".pdf"), mode = "wb") # Apply the download function
    
  pdf_txt <- pdf_text(paste0("file",i,".pdf"))
  txt <- str_c(pdf_txt, collapse = "\n")
  date <- str_extract(txt, "Dispatch.*Thru: \\d{2}/\\d{2}/\\d{4}")
  name_pdf <- paste0(str_replace_all(date,"/","_") %>% str_replace_all(" ","") %>% str_replace_all(":",""),".pdf")
  name_txt <- paste0(str_replace_all(date,"/","_") %>% str_replace_all(" ","") %>% str_replace_all(":",""),".txt")
  
  file.rename(paste0("file",i,".pdf"),name_pdf)
  write(pdf_txt, name_txt)
    },error = function(e){}
    )
 
  i = i + 1
} 
print("Finished Page 1")
#move to the next consequent pages 


# getting the number of pages in the website to cycle through
pagenum <- read_html(first_pg) %>% 
            html_nodes(".page-numbers") %>%                    # find all links for the pages
              html_attr("href") %>%                           # extracts the urls
                .[!is.na(.)] %>% 
                  str_extract_all("page/[[:digit:]]{1}") %>%
                    str_replace_all("page/","") %>%
                      max(unique(.))
p = 2
root = "http://www.brocktonpolice.com/category/police-log/page/" #base url to add page numbers
while(p <= pagenum) {
  url = paste0(root,p)
 
  
  page_links <- read_html(url) %>% 
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     # get the url
    str_subset("\\.pdf")
  i = 1
  while (i <= length(page_links)) { 
      # find those that end in .pdf
    print(paste0("working on file ", i, "/",length(page_links), " on page ", p))
      tryCatch({
    page_links[i] %>%
      download.file(paste0("file",i,".pdf"), mode = "wb") # Apply the download function
    
    #renaming the files as its being downloaded
    pdf_txt <- pdf_text(paste0("file",i,".pdf"))
    txt <- str_c(pdf_txt, collapse = "\n")
    date <- str_extract(txt, "Dispatch.*Thru: \\d{2}/\\d{2}/\\d{4}")
    name_pdf <- paste0(str_replace_all(date,"/","_") %>% str_replace_all(" ","") %>% str_replace_all(":",""),".pdf")
    name_txt <- paste0(str_replace_all(date,"/","_") %>% str_replace_all(" ","") %>% str_replace_all(":",""),".txt")
    
    file.rename(paste0("file",i,".pdf"),name_pdf)
    write(pdf_txt, name_txt)
      },error = function(e){}
      )
    i = i + 1
  } 
  print(paste0("Finished Page ", p))
  p = p + 1
}
