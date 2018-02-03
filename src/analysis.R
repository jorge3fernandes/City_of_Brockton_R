library(ggplot2)
library(tidyr)#
library(dplyr)
library(stringr)
library(fuzzyjoin)
library(stringdist)
library(ggmap)
library(zoo)
library(networkD3)
library(network)

if (exist('../crawler_result_Conversion/*.txt')){
  allTxt <- getAllTxt()
}


############# Netword Graph Function ############
networkfun = function(){
  Colname = readline("Please type Summons, Arrested, or police_officer to see the Network Chart: ")

  if(!(Colname %in% c("Summons", "Arrested", "police_officer"))){
    print("Please choose wisely: Summons, Arrested, or police_officer")
    break()
  }

  disptch_data <- read.csv("Dispatch.csv", stringsAsFactors = FALSE)
  newDF <- select(disptch_data,ID,Colname) %>% na.omit()

  ####need to improve this chunk to not have hard coded column names####

  lrgst_row = subset(newDF, nchar(newDF[,2]) == max(nchar(newDF[,2])))
  #counting the max number of columns based on the longest string
  if_else(Colname == "police_officer", 
          str_extract_all(lrgst_row,",")[[2]],
          str_extract_all(lrgst_row,"\", \"")[[2]])

  if ( Colname == "police_officer"){
    field_view = separate(newDF, col = Colname, 
                          into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10",
                                   "Name11", "Name12", "Name13", "Name14", "Name15", "Name16", "Name17", "Name18", "Name19", "Name20",
                                   "Name21", "Name22", "Name23", "Name24", "Name25", "Name26", "Name27", "Name28", "Name29", "Name30",
                                   "Name31", "Name32", "Name33", "Name34", "Name35", "Name36", "Name37", "Name38", "Name39", "Name40"), 
                          #sep = "\", \"")  
                          sep = ",")
  } else{
    field_view = separate(newDF, col = Colname, 
                          into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10",
                                   "Name11", "Name12", "Name13", "Name14", "Name15", "Name16", "Name17", "Name18", "Name19", "Name20",
                                   "Name21", "Name22", "Name23", "Name24", "Name25", "Name26", "Name27", "Name28", "Name29", "Name30",
                                   "Name31", "Name32", "Name33", "Name34", "Name35", "Name36", "Name37", "Name38", "Name39", "Name40"), 
                          sep = "\", \"")
  }
  #clearing columns with all NA

  #field_view = field_view[, colSums(is.na(field_view)) != nrow(field_view)]
  field_view = gather(field_view, key = "x", value = field, Name1:Name40)
  field_view = na.omit(field_view)
  field_view$x <- NULL
  ##### 
  field_view_Adj <- dplyr::inner_join(field_view, field_view, by = "ID")[,-1]
  field_view_Adj <- subset(field_view_Adj, field.x != field.y)
  # field_view_Adj <- apply(field_view_Adj, 2, as.character)
  # 
  # field.network <- network(field_view_Adj, directed = FALSE)
  # as.sociomatrix(field.network)
  # as.edgelist(field.network)
  # plot(field.network, label = field.network%v%"vertex.names")
  # 
  # newDF %>% mutate_if(is.factor, as.character) -> newDF

  field_view_Adj$field.y = str_replace_all(field_view_Adj$field.y, "\"","") %>% trimws(which = "both")
  field_view_Adj$field.x = str_replace_all(field_view_Adj$field.x, "\"","") %>% trimws(which = "both")
  simpleNetwork(field_view_Adj, zoom = TRUE)
}

# Create the network graph
networkfun()
