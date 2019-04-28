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

column <- "officer"
sep <- ","

############# Netword Graph Function ############
networkfun = function(column, sep){
  
  officer <- splitstackshape::cSplit(dataTotal_clean, column, sep, drop = FALSE) %>%
    select(starts_with("officer"))
    
  idx <- t(combn(seq_along(officer), 2))
  edgelist <- lapply(1:nrow(idx), function(i) officer[, c(idx[i, 1], idx[i, 2])])
  edgelist <- lapply(edgelist, setNames, c("ID1","ID2"))
  edgelist <- do.call(rbind, edgelist)
  edgelist <- edgelist[rowSums(is.na(edgelist))==0, ]
  edgelist
  
  
  officer.edge <- gather(officer, key = "x", value = "value", names(select(officer,starts_with("officer")))) %>% 
    na.omit()
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
