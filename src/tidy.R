dataCleaner <- function(dataframe){
  # Reads in the raw dataframe, cleans, and outputs it
  #
  # Args:
  #   dataframe: the output of the pdftoTable function
  #
  # Returns: Returns a clean dataset
  
  dataframe[] <- lapply(dataframe, as.character) # 1st let's make sure we set all columns as character vectors
  dataTotal_clean <- fill(dataframe, Date) %>% # Fill NA rows with their corresponding dates
    subset(Time != 'character(:0)') # deleting rows where Time equals character(:0)
  dataTotal_clean[dataTotal_clean == c("character(0)")] <- NA # Replace character(0) with NA accross all columns
  dataTotal_clean[dataTotal_clean == c("character(0), BROCKTON, MA")] <- NA # Replace "character(0), BROCKTON, MA" with NA accross all columns
  dataTotal_clean$timeStamp <- paste0(dataTotal_clean$Date,' ', dataTotal_clean$Time) # create a time stampfield
  dataTotal_clean[] <- as.data.frame(sapply(dataTotal_clean, str_replace_all, pattern="c\\(|\\\\n|\"|\\)", replacement="")) %>%  # Using regex to clean unnecessary characters from data
    lapply(trimws,which = "both") # Removing all leading and trailing whitespaces from all columns
  dataTotal_clean <- separate(dataTotal_clean, callReasonAction, sep = "    ",  extra = "merge", into = c("callReason", "action"), remove = TRUE, fill = "right") %>%
    unique()
  dataTotal_clean$action <- trimws(dataTotal_clean$action, which = "both")
  write.csv(dataTotal_clean, "./data/cleanData.csv", row.names = FALSE)
  return(dataTotal_clean)
}


############# Update Address look-up table ##########
latLonUpdate <- function(dataframe){
  # Reads the cleaned dataframe from dataCleaner() and updates the lattitude and longitudes for each address
  # Args:
  #   dataframe: the output of the dataCleaner() function
  #
  # Returns: Returns a clean dataset with lat lon attached
  
  gg_address_view <- read.csv("./data/gg_address.csv", stringsAsFactors = FALSE) # reading in the geocoded addresses
  gg_address_view[] <- lapply(gg_address_view, as.character) # ensuring all the columns are converted to characters
  
  distinct_address <- as.character(unique(dataframe$addressGeo)) #getting distinct addresses from the recently parsed data
  
  new_address <- distinct_address[!(distinct_address %in% gg_address_view$Actual_Address)] #checking for addresses that haven't been geocoded
  
  #geocoding the new addresses and adding them to the database
  gg_new_address  <- tryCatch({
    geocode(new_address[1:2499], output = "more")  %>% # using google to get lat lon
      select(lat,lon,type,address) # selecting desired columns
  }, error = function(e){})
  
  names(gg_new_address)  <- c("lat","lon","location_type", "formatted") # renaming the columns
  
  
  
  gg_new_address$Actual_Address <- new_address # Appending original addresses so we can join later to the dataset
  gg_new_address <- subset(gg_new_address, !is.na(lat)) # deleting all the records where we couldn't find a match
  
  gg_address_view <- smartbind(gg_address_view,gg_new_address) %>% unique()# appending the new geocoded addresses to the address lookup data
  
  test <- left_join(dataTotal_clean, gg_address_view, by = c("addressGeo" = "Actual_Address") )
  
  write.csv(gg_new_address, "./data/gg_address.csv", row.names = FALSE) # saving the address view
  write.csv(gg_new_address, "./data/gg_address.csv", row.names = FALSE) # saving the address view
}