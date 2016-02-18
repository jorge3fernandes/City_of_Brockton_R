website <- 'http://brocktonpolice.com/online-reporting/'

thepage <- readLines(website)
head(thepage)

require(XML)
pagerender <- htmlParse(thepage)
