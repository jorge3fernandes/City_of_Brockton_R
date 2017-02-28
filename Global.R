library(ggmap)
devtools::install_github("hrbrmstr/nominatim")
library(nominatim)
library(dplyr)

folder <- "/Users/legs_jorge/Documents/Data Science Projects/RBrockton/Police_logs"
setwd(folder)
Disp_2017 <- read.csv("full_bpd_calls.csv", row.names = NULL)
Disp_2017$Date <- as.POSIXct(Disp_2017$Date)

Disp_2017 <- subset(Disp_2017, Date >= as.POSIXct("2017-01-01"))

