library(rvest)
library(stringr)



root <- read_html("http://www.brocktonpolice.com/category/police-log/")

##### Collecting all the links on the side bar ####
##### Using Selector Gadget to find the right nodes

sidebar_links <- root %>% 
                  html_nodes("#sidebar :nth-child(1)") %>% #finds all links on the sidebar
                  html_attr("href") %>% #extracts the urls
                  .[!is.na(.)] # removes NA
r <- 0 #to generate sequence to the files downloaded
##### Following the links to download the PDFs####
for (i in seq_along(sidebar_links)) {
        nav <- read_html(sidebar_links[i]) %>% html_nodes(".alignleft") %>% html_attr("href")
       #after clicking on the sidebar links 
        for (j in seq_along(nav)){
                r <- r + 1
                tryCatch({
                  read_html(nav[j]) %>%
                  html_nodes("a") %>%       # find all links
                  html_attr("href") %>%     # get the url
                  str_subset("\\.pdf") %>%  # find those that end in pdf
                  
                    download.file(paste0("file",r,".pdf"), mode = "wb")
                    
                  },error = function(e){}
                  )
                  
        }
}



























