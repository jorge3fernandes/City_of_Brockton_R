library(ggmap)

Full_df <- read.csv("full_bpd_calls.csv")
Full_df$address <- as.character(Full_df$address)

infile <- "input"
tempfilename <- paste0(infile, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(Full_df$address))){
  print(paste("Working on index", ii, "of", length(Full_df$address)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(Full_df$address[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}
