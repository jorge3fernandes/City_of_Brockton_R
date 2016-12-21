library(ggploRefer_To_Summons)
library(tidyr)
library(dplyr)
library(plotly)
library(sqldf)
library(fuzzyjoin)
# Cleanning the data for analysis

setwd("/Users/legs_jorge/Documents/Data Science Projects/RBrockton/Police_logs")
clean_data <- read.csv("full_bpd_calls.csv")

clean_data$X <- NULL

##################### Use sqldf to do the join next time you start on the project
#unmerging cells with multiple values and turning them into their specific rows

################# Summons

Summonsdf <- clean_data[,c("Date","Summons","charges")]
colnames(Summonsdf) <- c("Date","Summons","charges.summons")
Summonsdf = na.omit(Summonsdf)

Summonsdf = separate(Summonsdf, col = Summons, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10"), sep = "\", \"")
Summonsdf = gather(Summonsdf, key = "x", value = "Summons", Name1:Name10)
Summonsdf = na.omit(Summonsdf)

############### Refer_To_Summons
Refer_To_Summonsdf <- clean_data[,c("Date","Refer_To_Summons")] 
Refer_To_Summonsdf = na.omit(Refer_To_Summonsdf)

Refer_To_Summonsdf = separate(Refer_To_Summonsdf, col = Refer_To_Summons, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10"), sep = ",")
Refer_To_Summonsdf = gather(Refer_To_Summonsdf, key = "x", value = "Refer_To_Summons", Name1:Name10)
Refer_To_Summonsdf = na.omit(Refer_To_Summonsdf)

Summons.Refer <- full_join(Summonsdf,Refer_To_Summonsdf, by= c("x"="x" , "Date"= "Date"))

##################Arrest#############################

Arrestdf <- clean_data[,c("Date","Arrested","charges")]
colnames(Arrestdf) <- c("Date","Arrested","charges.Arrest")
Arrestdf = na.omit(Arrestdf)

Arrestdf = separate(Arrestdf, col = Arrested, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10"), sep = "\", \"")
Arrestdf = gather(Arrestdf, key = "x", value = "Arrested", Name1:Name10)
Arrestdf = na.omit(Arrestdf)

############## Refer_To_Arrest
Refer_To_Arrestdf <- clean_data[,c("Date","Refer_To_Arrest")]
Refer_To_Arrestdf = na.omit(Refer_To_Arrestdf)

Refer_To_Arrestdf = separate(Refer_To_Arrestdf, col = Refer_To_Arrest, into = c("Name1", "Name2", "Name3", "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10"), sep = ",")
Refer_To_Arrestdf = gather(Refer_To_Arrestdf, key = "x", value = "Refer_To_Arrest", Name1:Name10)
Refer_To_Arrestdf = na.omit(Refer_To_Arrestdf)

Arrest.Refer <- full_join(Arrestdf,Refer_To_Arrestdf, by= c("x"="x" , "Date"= "Date"))

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

Age.Address <- full_join(Agedf2,susp.Addressdf2, by = c("x"="x" , "Date"= "Date"))

individual.full <- full_join(Arrest.Summons,Age.Address, by= c("x"="x" , "Date"= "Date"))

write.csv(individual.full,"individual.full.csv", row.names = FALSE)

