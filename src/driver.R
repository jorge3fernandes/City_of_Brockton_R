library(tidyr)
library(dplyr)
library(gtools)

source("src/parser.R")
source("src/crawler.R")

startTimer <- proc.time()
firstPg <- "http://www.brocktonpolice.com/category/police-log/" 
PgPrefix <- "http://www.brocktonpolice.com/category/police-log/page/"

# Scrape the website and get the links to the PDFs - Sourced from crawler.r
AllLinks <- GetAllLinks(firstPg,PgPrefix)

# Convert all PDFs to a tidy format - sourced from parser.r
dataTotal <- do.call("rbind", lapply(AllLinks[1:3], pdfToTable))

############# Cleaning the dataset ############# 

dataTotal[] <- lapply(dataTotal, as.character) # 1st let's make sure we set all columns as character vectors
dataTotal <- fill(dataTotal,Date) %>% # Fill NA rows with their corresponding dates
                 subset(Time != 'character(:0)') # deleting rows where Time equals character(:0)
dataTotal[dataTotal==c("character(0)")] <- NA # Replace character(0) with NA accross all columns

# Using regex to clean unnecessary characters from data
dataTotal <- as.data.frame(sapply(dataTotal,str_replace_all,pattern="c\\(|\\\\n|\"|\\)|character\\(0, BROCKTON, MA",replacement=""))


############# Update Address look-up table ##########

gg_address_view <- read.csv("gg_address.csv", stringsAsFactors = FALSE) #reading in the geocoded addresses
gg_address_view[] <- lapply(gg_address_view, as.character) #ensuring all the columns are converted to characters

distinct_address <- unique(dataTotal$addressGeo) #getting distinct addresses from the recently parsed data

new_address <- distinct_address[!(distinct_address %in% gg_address_view$Actual_Address)] #checking for addresses that haven't been geocoded

#geocoding the new addresses and adding them to the database
gg_new_address  <- tryCatch({
                      geocode(new_address, output = "more")  %>% # using google to get lat lon
                          select(lat,lon,type,address) # selecting desired columns
                    },error = function(e){})

names(gg_new_address)  <- c("lat","lon","location_type", "formatted") # renaming the columns



gg_new_address$Actual_Address <- new_address # Appending original addresses so we can join later to the dataset
gg_new_address <- subset(gg_new_address, !is.na(lat)) # deleting all the records where we couldn't find a match

gg_address_view <- smartbind(gg_address_view,gg_new_address) %>% unique()# appending the new geocoded addresses to the address lookup data

write.csv(gg_new_address, "gg_address.csv", row.names = FALSE) # saving the address view

