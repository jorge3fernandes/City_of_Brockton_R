library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(fuzzyjoin)
library(stringdist)
library(ggmap)
library(zoo)
# Cleanning the data for analysis
# Using a year due to geocoding limitations

setwd("/Users/legs_jorge/Documents/Data Science Projects/RBrockton/Police_logs")
clean_data <- subset(fnl_data, as.yearmon(fnl_data$timeStamp) >= as.yearmon(Sys.Date()) - 1/12)



addresses_view <- unique(clean_data[,c("Response_address","address_Geo")]) 
# Geocoding script for large list of addresses.


# get the input data
infile <- "input1"
data <- addresses_view

# get the address list, and append "Ireland" to the end to increase accuracy 
# (change or remove this if your address already include a country etc.)
addresses = data$address_Geo
addresses = paste0(addresses, ", United States")

#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

#now we add the latitude and longitude to the main data
data$lat <- geocoded$lat
data$long <- geocoded$lat
data$accuracy <- geocoded$accuracy

#finally write it all to the output files
saveRDS(data, paste0("../data/", infile ,"_geocoded.rds"))
#write.table(data, file=paste0("../data/", infile ,"_geocoded.csv"), sep=",", row.names=FALSE)


##################### Use sqldf to do the join next time you start on the project
#unmerging cells with multiple values and turning them into their specific rows

################# Summons

Summonsdf <- fnl_data[,c("date","Summons")]
colnames(Summonsdf) <- c("date","Summons")

Summons_view = separate(Summonsdf, col = Summons, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10",
                                                           "Name11", "Name12", "Name13", "Name14", "Name15", "Name16", "Name17", "Name18", "Name19", "Name20",
                                                           "Name21", "Name22", "Name23", "Name24", "Name25", "Name26", "Name27", "Name28", "Name29", "Name30"), sep = "\", \"")
Summons_view = gather(Summons_view, key = "x", value = "Summons", Name1:Name30)
Summons_view = na.omit(Summons_view)

Summonsdf %>% mutate_if(is.factor, as.character) -> Summonsdf

test_data <- clean_data[1:500,]
#test_data <- na.omit(test_data)

test_data %>% mutate_if(is.factor, as.character) -> test_data
#test_data[is.na(test_data)] <- "Data Not available"

test_data1 <- test_data %>%
  regex_left_join( Summonsdf, by = c(Summons = "Summons", Date = "Date")) %>% unique()




############### Refer_To_Summons
Refer_To_Summonsdf <- fnl_data[,c("date","Refer_To_Summons", "charges")] 
Refer_To_Summonsdf = na.omit(Refer_To_Summonsdf)

Refer_To_Summons_view = separate(Refer_To_Summonsdf, col = Refer_To_Summons, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10",
                                                                                   "Name11", "Name12", "Name13", "Name14", "Name15", "Name16", "Name17", "Name18", "Name19", "Name20",
                                                                                   "Name21", "Name22", "Name23", "Name24", "Name25", "Name26", "Name27", "Name28", "Name29", "Name30"), sep = ",")
Refer_To_Summons_view = gather(Refer_To_Summons_view, key = "x", value = "Refer_To_Summons", Name1:Name30)
Refer_To_Summons_view = na.omit(Refer_To_Summons_view)

##################Arrest#############################

Arrestdf <- fnl_data[,c("date","Arrested")]
colnames(Arrestdf) <- c("date","Arrested")
Arrestdf = na.omit(Arrestdf)

Arrest_view = separate(Arrestdf, col = Arrested, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10",
                                                       "Name11", "Name12", "Name13", "Name14", "Name15", "Name16", "Name17", "Name18", "Name19", "Name20",
                                                       "Name21", "Name22", "Name23", "Name24", "Name25", "Name26", "Name27", "Name28", "Name29", "Name30"), sep = "\", \"")
Arrest_view = gather(Arrest_view, key = "x", value = "Arrested", Name1:Name30)
Arrest_view = na.omit(Arrest_view)

############## Refer_To_Arrest
Refer_To_Arrestdf <- fnl_data[,c("date","Refer_To_Arrest")]
Refer_To_Arrestdf = Refer_To_Arrestdf[!is.na(Refer_To_Arrestdf$Refer_To_Arrest),]

Refer_To_Arrest_view = separate(Refer_To_Arrestdf, col = Refer_To_Arrest, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10",
                                                                                "Name11", "Name12", "Name13", "Name14", "Name15", "Name16", "Name17", "Name18", "Name19", "Name20",
                                                                                "Name21", "Name22", "Name23", "Name24", "Name25", "Name26", "Name27", "Name28", "Name29", "Name30"), sep = ",")
Refer_To_Arrest_view = gather(Refer_To_Arrest_view, key = "x", value = "Refer_To_Arrest", Name1:Name30)
Refer_To_Arrest_view = na.omit(Refer_To_Arrest_view)

################## Age ##################

Agedf <- fnl_data[,c("date","Age")]
Agedf = na.omit(Agedf)

Aged_view = separate(Agedf, col = Age, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10",
                                                "Name11", "Name12", "Name13", "Name14", "Name15", "Name16", "Name17", "Name18", "Name19", "Name20",
                                                "Name21", "Name22", "Name23", "Name24", "Name25", "Name26", "Name27", "Name28", "Name29", "Name30"), sep = ",")
Aged_view = gather(Aged_view, key = "x", value = "Age", Name1:Name30)
Aged_view = na.omit(Aged_view)

## Suspect_Address
susp.Addressdf <- fnl_data[,c("date","Suspect_Address")]
susp.Addressdf = na.omit(susp.Addressdf)
susp.Address_view = separate(susp.Addressdf, col = Suspect_Address, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10",
                                                                           "Name11", "Name12", "Name13", "Name14", "Name15", "Name16", "Name17", "Name18", "Name19", "Name20",
                                                                           "Name21", "Name22", "Name23", "Name24", "Name25", "Name26", "Name27", "Name28", "Name29", "Name30"), sep = "\", [[:digit:]]{1}\"")
susp.Address_view = gather(susp.Address_view, key = "x", value = "Suspect_Address", Name1:Name30)
susp.Address_view = na.omit(susp.Address_view)

susp.Address_view$Suspect_Address <- str_replace_all(susp.Address_view$Suspect_Address, "c\\(\\\"    ","") %>% str_replace_all("\\\n\""," : ") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")



################ Putting it all together #####################

individual.full <- full_join(Agedf2, Arrest.Summons, by = c("x" = "x" , "Date" = "Date"))

################ Making the Complete Dataset #################

write.csv(individual.full,"individual.full.csv", row.names = FALSE)
colnames(clean_data)
colnames(individual.full)
clean_dataT <- clean_data
clean_dataT[is.na(clean_dataT)] <- 'Data Not Available'
individual <- stringdist_full_join(clean_data, individual.full, by = c("Summons" = "Summons" , "Date" = "Date", "Refer_To_Summons" = "Refer_To_Summons",
                                                                       "Arrested" = "Arrested", "Refer_To_Arrest" = "Refer_To_Arrest", "Age" = "Age"))

###############Joinning everything together ###########


test_data1 <- test_data %>%
  regex_left_join( Summonsdf, by = c(Summons = "Summons", Date = "Date")) %>% unique()


test_data1 <- test_data %>%
  regex_left_join( Summonsdf, by = c(Summons = "Summons", Date = "Date")) %>% unique()


test_data1 <- test_data %>%
  regex_left_join( Summonsdf, by = c(Summons = "Summons", Date = "Date")) 


test_data1 <- test_data %>%
  regex_left_join( Summonsdf, by = c(Summons = "Summons", Date = "Date")) 
