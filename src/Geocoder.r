
hereGeocoder <- function(address){
  library(jsonlite)
  library(httr)
  #address <- "N MAIN ST &  ELLIOT ST, BROCKTON, MA" # for testing purposes
  addressFormat <- str_replace_all(address," ","+")
  app_id <- "sDhRf65tLCXkGHdCd05r"
  app_code <- "7-zSaacZV1gEcsaCWoIbBA"
  
  base <- "https://geocoder.api.here.com/6.2/geocode.json?app_id="
  call1 <- paste0(base,app_id,"&app_code=",app_code,"&searchtext=", addressFormat)
  
  get_latlon <-  GET(call1)
  
  get_latlon_content <-  content(get_latlon, "text")
  
  get_latlon_json <- fromJSON(get_latlon_content, flatten = TRUE)
  
  get_latlon_df <- tryCatch({
    as.data.frame(get_latlon_json)
  },error = function(e){
    
  })
  
  if (is.null(get_latlon_df)){
    final <- data.frame(formatted = NA, lat = NA, lon = NA, addressGeo = address)
  }else{
    final <- get_latlon_df$Response.View.Result %>%
      as.data.frame()
    final <- data.frame(formatted = final$Location.Address.Label, 
                        lat = final$Location.DisplayPosition.Latitude,
                        lon = final$Location.DisplayPosition.Longitude,
                        addressGeo = address)
    #colnames(final) <- c("formatted","lat", "lon", "addressGeo")
      
  #     as.data.frame(get_latlon_df$Response.View.Result) %>% 
  #     select(Location.Address.Label, Location.DisplayPosition.Latitude, Location.DisplayPosition.Longitude )
  #   colnames(final) <- c("addressFormatted","lat", "lon")
  #   final$address <- address
  }        
  
  return(final)
}

# Teste
# hereGeocoder("0 MANLEY ST, BROCKTON, MA @ 1012 BELMONT ST, BROCKTON, MA")




