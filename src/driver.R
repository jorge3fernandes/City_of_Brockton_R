library(stringr)   # For text manipulation
library(testthat)  # For unit testing: test_that
library(pdftools)  # For PDF to Text Conversion
library(future.apply) # Helps with executing apply functions in parallel
library(tidyverse)
library(gtools)
library(data.table)
plan(multiprocess) ## Run in parallel on local computer

source("src/parser.R")
source("src/crawler.R")
source("src/Geocoder.R")
source("src/tidy.R")

start_time <- Sys.time()
############# Gathering all links ############# 

firstPg <- "http://www.brocktonpolice.com/category/police-log/" 
PgPrefix <- "http://www.brocktonpolice.com/category/police-log/page/"

# Scrape the website and get the links to the PDFs - Sourced from crawler.r

AllLinks <- GetAllLinks(firstPg, PgPrefix)

############# Transforming all PDFs into a tidy format ############# 
# Convert all PDFs to a tidy format - sourced from parser.r

dataTotal <- future_lapply(AllLinks, pdfToTable) %>%
  rbindlist()

print("Done creating the table")

############# Cleaning the dataset ############# 
print("Cleaning and preparing the address column for geocoding")

dataTotal_clean <- dataCleaner(dataTotal)

############# Update Address look-up table ##########
print("Geocoding New Addresses and Updating the Address Look-up table")

distinct_address <- as.character(unique(dataTotal_clean$addressGeo)) %>% #getting distinct addresses from the recently parsed data
  na.omit()

if(file.exists("./data/geocoded_address_lookup.csv")){
  
  geocoded_address_lookup <- read.csv("./data/geocoded_address_lookup.csv", stringsAsFactors = FALSE) # reading in the geocoded addresses
  geocoded_address_lookup[] <- future_lapply(geocoded_address_lookup, as.character) # ensuring all the columns are converted to characters
  new_address <- distinct_address[!(distinct_address %in% geocoded_address_lookup$Actual_Address)] #checking for addresses that haven't been geocoded
  
}else{
  geocoded_address_lookup <- data.frame(formatted = NULL, lat = NULL, lon = NULL, Actual_Address = NULL)
}

new_address <- distinct_address

print("Geocoding New Addresses and Updating the Address Look-up table")

here_geocoded_Address <- future_lapply(new_address, hereGeocoder) %>%
  rbindlist()


here_geocoded_Address <- subset(here_geocoded_Address, !is.na(lat)) # deleting all the records where we couldn't find a match

geocoded_address_lookup <- smartbind(geocoded_address_lookup, here_geocoded_Address) %>% 
  unique()# appending the new geocoded addresses to the address lookup data

end_time <- Sys.time()
end_time - start_time
write.csv(geocoded_address_lookup, "geocoded_address_lookup.csv", row.names = FALSE) # saving the address view

