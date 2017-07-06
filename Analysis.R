library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(fuzzyjoin)
library(stringdist)
# Cleanning the data for analysis

setwd("/Users/legs_jorge/Documents/Data Science Projects/RBrockton/Police_logs")
clean_data <- read.csv("full_bpd_calls.csv")


##################### Use sqldf to do the join next time you start on the project
#unmerging cells with multiple values and turning them into their specific rows

################# Summons

Summonsdf <- clean_data[,c("Date","Summons")]
colnames(Summonsdf) <- c("Date","Summons")

Summonsdf = separate(Summonsdf, col = Summons, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10"), sep = "\", \"")
Summonsdf = gather(Summonsdf, key = "x", value = "Summons", Name1:Name10)
Summonsdf = na.omit(Summonsdf)

Summonsdf %>% mutate_if(is.factor, as.character) -> Summonsdf
#Summonsdf[is.na(Summonsdf)] <- "Data Not available"

test_data <- clean_data[1:500,]
#test_data <- na.omit(test_data)

test_data %>% mutate_if(is.factor, as.character) -> test_data
#test_data[is.na(test_data)] <- "Data Not available"

test_data1 <- test_data %>%
  regex_left_join( Summonsdf, by = c(Summons = "Summons", Date = "Date")) %>% unique()




############### Refer_To_Summons
Refer_To_Summonsdf <- clean_data[,c("Date","Refer_To_Summons", "charges")] 
Refer_To_Summonsdf = na.omit(Refer_To_Summonsdf)

Refer_To_Summonsdf = separate(Refer_To_Summonsdf, col = Refer_To_Summons, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10"), sep = ",")
Refer_To_Summonsdf = gather(Refer_To_Summonsdf, key = "x", value = "Refer_To_Summons", Name1:Name10)
Refer_To_Summonsdf = na.omit(Refer_To_Summonsdf)

Refer_To_Summonsdf %>% mutate_if(is.factor, as.character) -> Refer_To_Summonsdf
test_data1 %>% mutate_if(is.factor, as.character) -> test_data1

Summons.Refer <- full_join(Summonsdf,Refer_To_Summonsdf, by = c("x" = "x" , "Date" = "Date"))

##################Arrest#############################

Arrestdf <- clean_data[,c("Date","Arrested")]
colnames(Arrestdf) <- c("Date","Arrested")
Arrestdf = na.omit(Arrestdf)

Arrestdf = separate(Arrestdf, col = Arrested, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10"), sep = "\", \"")
Arrestdf = gather(Arrestdf, key = "x", value = "Arrested", Name1:Name10)
Arrestdf = na.omit(Arrestdf)

############## Refer_To_Arrest
Refer_To_Arrestdf <- clean_data[,c("Date","Refer_To_Arrest", "charges")]
Refer_To_Arrestdf = Refer_To_Arrestdf[!is.na(Refer_To_Arrestdf$Refer_To_Arrest),]

Refer_To_Arrestdf = separate(Refer_To_Arrestdf, col = Refer_To_Arrest, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10"), sep = ",")
Refer_To_Arrestdf = gather(Refer_To_Arrestdf, key = "x", value = "Refer_To_Arrest", Name1:Name10)
Refer_To_Arrestdf = na.omit(Refer_To_Arrestdf)

Arrest.Refer <- full_join(Arrestdf,Refer_To_Arrestdf, by = c("x" = "x" , "Date" = "Date"))

#################Merging Arrests and Summons#################


Arrest.Summons <- full_join(Summons.Refer,Arrest.Refer, by= c("x"="x" , "Date"= "Date"))


##################Age#############################

Agedf <- clean_data[,c("Date","Age")]
Agedf = na.omit(Agedf)

Agedf1 = separate(Agedf, col = Age, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10"), sep = ",")
Agedf2 = gather(Agedf1, key = "x", value = "Age", Name1:Name10)
Agedf2 = na.omit(Agedf2)

## Suspect_Address
susp.Addressdf <- clean_data[,c("Date","Suspect_Address")]
susp.Addressdf = na.omit(susp.Addressdf)
susp.Addressdf1 = separate(susp.Addressdf, col = Suspect_Address, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10"), sep = "\", \"")
susp.Addressdf2 = gather(susp.Addressdf1, key = "x", value = "Suspect_Address", Name1:Name10)
susp.Addressdf2 = na.omit(susp.Addressdf2)

susp.Addressdf2$Suspect_Address <- str_replace_all(susp.Addressdf2$Suspect_Address, "c\\(\\\"    ","") %>% str_replace_all("\\\n\""," : ") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")



################ Putting it all together #####################

individual.full <- full_join(Agedf2,Arrest.Summons, by = c("x" = "x" , "Date" = "Date"))

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
