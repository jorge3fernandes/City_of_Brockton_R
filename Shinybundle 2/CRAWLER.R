
#############Practice##############
require(XML)

#reading all the tables from the website and choosing the first one (which = 1)

bowl <- readHTMLTable("http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/", header = FALSE, stringAsfactors = FALSE, which = 1)

bowl


## Reading the data by parsing and using the Xpath.
#getting the address. By inspecting the element we can see where the address is located withing the html

address <- "http://www.menupages.com/restaurants/fiores-pizza/menu"
thepage <- readLines(address)
head(thepage)
require(XML)
pagerender <- htmlParse(thepage)

address <- xpathApply(pagerender,"//li[@class='address adr']/span[@class='addr street-address']",xmlValue)[[1]]
address

city <- xpathApply(pagerender,"//li[@class='address adr']/span/span[@class='locality']")[[1]]
city

headers <- xpathSApply(pagerender, "//*[@id='restaurant-menu']/h3",xmlValue)
headers

items <- xpathSApply(pagerender, "//table[starts-with(@class,prices-)]")
items

items <- lapply(items,readHTMLTable, stringAsFactors = F)
head(items)

require(plyr)
menu <- "http://www.menupages.com/restaurants/all-areas/all-neighborhoods/pizza/"
doc <- htmlParse(menu)

placeNameLink <- xpathApply(doc, "//table/tr/td[@class='name-address']/a[@class='link']",
                fun = function(x){c(Name = xmlValue(x,recursive = FALSE),
                                      Link = xmlAttrs(x)[2])})
placeNameLink

placeNameLink <- ldply(placeNameLink)

head(placeNameLink)

#comments
require(XML)
teafile <- "http://www.jaredlander.com/data/SocialComments.xml"
teaParsed <- xmlToList(teafile)

length(teaParsed)
str(teaParsed)
teaParsed[[1]][[1]]$id
teaParsed[[1]][[1]]$author$name
teaParsed[[1]][[1]]$published
teaParsed[[1]][[1]]$content$.attrs
teaParsed[[1]][[1]]$content$.attrs[["sentimentScore"]]



# How to download PDF
url <- 'http://brocktonpolice.com/wp-content/uploads/2016/02/2116.pdf'
download.file(url, 'BPF_FEBRUARY.pdf', mode = 'wb')


#####################RSelenium############################

install.packages("RSelenium")
library("RSelenium")

startServer()

checkForServer()

mybrowser <- remoteDriver(browserName = "chrome")

mybrowser$open()



x = as.character(c(3:5))
y = as.character(c(6:9)) 

as.list(str_c(x[1],y))




























