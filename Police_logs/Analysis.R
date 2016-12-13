library(ggplot2)
library(tidyr)
library(dplyr)

# Cleanning the data for analysis

setwd("/Users/legs_jorge/Documents/Data Science Projects/RBrockton/Police_logs")
clean_data <- read.csv("full_bpd_calls.csv")

clean_data$X <- NULL

# focusing on the data where people where summond or arrested

crimes <- subset(clean_data, !is.na(clean_data$Refer_To_Summons) | !is.na(clean_data$Refer_To_Arrest) )
crimes$Date <- as.POSIXct(crimes$Date)

ggplot(crimes, aes(weekdays(Date),count(Date))+geom_bar())

Date_sum <- count(crimes, as.Date(Date))
colnames(Date_sum) <- c('Date', 'Count')

Week_sum <- count(crimes, weekdays(Date))

colnames(Week_sum) <- c('Weekdays', 'Count')
Week_sum$Weekdays <- factor(Week_sum$Weekdays, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
Week_sum <- arrange(Week_sum, Weekdays)
plot_ly(Week_sum, x = Weekdays, y = Count)
ggplot(Date_sum, aes(Date, Count)) + geom_line() + xlab("") + ylab("Daily Count")

#unmerging cells with multiple values and turning them into their specific rows
SummonsDF <- clean_data %>% 
  mutate(Summons = strsplit(as.character(Summons), "\", \"")) %>% 
  unnest(Summons)

SummonsDF <- SummonsDF[,c("Date","Refer_To_Summons","Summons")]
  
SummonsDF$Summons <- str_replace_all(SummonsDF$Summons, "\"","")

ArrestedDF <- clean_data %>% 
  mutate(Arrested = strsplit(as.character(Arrested), "\", \"")) %>% 
  unnest(Arrested)
ArrestedDF <- ArrestedDF[,c("Date","Refer_To_Arrest" , "Arrested")]
ArrestedDF$Arrested <- str_replace_all(ArrestedDF$Arrested, "\"","")

Suspect_AddressDF <- clean_data %>% 
  mutate(Suspect_Address = strsplit(as.character(Suspect_Address), "\", \"")) %>% 
  unnest(Suspect_Address)

Suspect_AddressDF <- Suspect_AddressDF[,c("Date","Suspect_Address","Refer_To_Summons","Refer_To_Arrest")]
Suspect_AddressDF$Suspect_Address <- str_replace_all(Suspect_AddressDF$Suspect_Address,"c\\(\" ","") %>% str_replace_all("\\\\n","") %>% str_replace_all("\"\\)","")

test <- merge(x = Suspect_AddressDF, y = ArrestedDF, by = c("Date","Refer_To_Arrest"), all.x = TRUE)
