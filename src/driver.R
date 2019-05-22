library(tidyr)
library(dplyr)
library(gtools)
library(future.apply) # Helps with executing apply functions in parallel
plan(multiprocess) ## Run in parallel on local computer

source("src/parser.R")
source("src/crawler.R")
source("tidy.R")

############# Gathering all links ############# 

firstPg <- "http://www.brocktonpolice.com/category/police-log/" 
PgPrefix <- "http://www.brocktonpolice.com/category/police-log/page/"

# Scrape the website and get the links to the PDFs - Sourced from crawler.r

AllLinks <- GetAllLinks(firstPg, PgPrefix)

print("Done Gathering links")
############# Transforming all PDFs into a tidy format ############# 
# Convert all PDFs to a tidy format - sourced from parser.r
# 
# timerStart1 <- proc.time()
# dataTotal <- do.call("rbind", future_lapply(AllLinks, pdfToTable))
# runTime1 <- (proc.time() - timerStart1)/60
# runTime1

dataTotal <- do.call("rbind", lapply(AllLinks, pdfToTable))

print("Done creating a tidy data")

############# Cleaning the dataset ############# 

dataTotal_clean <- dataCleaner(dataTotal)


############# Update Address look-up table ##########

gg_address_view <- read.csv("gg_address.csv", stringsAsFactors = FALSE) # reading in the geocoded addresses
gg_address_view[] <- lapply(gg_address_view, as.character) # ensuring all the columns are converted to characters

distinct_address <- as.character(unique(dataTotal$addressGeo)) #getting distinct addresses from the recently parsed data

new_address <- distinct_address[!(distinct_address %in% gg_address_view$Actual_Address)] #checking for addresses that haven't been geocoded

#geocoding the new addresses and adding them to the database
gg_new_address  <- tryCatch({
                      geocode(new_address[1:2499], output = "more")  %>% # using google to get lat lon
                          select(lat,lon,type,address) # selecting desired columns
                    },error = function(e){})

names(gg_new_address)  <- c("lat","lon","location_type", "formatted") # renaming the columns



gg_new_address$Actual_Address <- new_address # Appending original addresses so we can join later to the dataset
gg_new_address <- subset(gg_new_address, !is.na(lat)) # deleting all the records where we couldn't find a match

gg_address_view <- smartbind(gg_address_view,gg_new_address) %>% unique()# appending the new geocoded addresses to the address lookup data

write.csv(gg_new_address, "gg_address.csv", row.names = FALSE) # saving the address view

