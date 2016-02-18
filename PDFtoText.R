# download pdftotxt from 
# ftp://ftp.foolabs.com/pub/xpdf/xpdfbin-win-3.03.zip
# and extract to your program files folder

# here is a pdf for mining
url <- "http://brocktonpolice.com/wp-content/uploads/2015/01/01202015.pdf"
dest <- tempfile(fileext = ".pdf")
download.file(url, dest, mode = "wb")

# set path to pdftotxt.exe and convert pdf to text
exe <- "C:\\Program Files\\xpdf\\bin32\\pdftotext.exe"
system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)

# get txt-file name and open it  
filetxt <- sub(".pdf", ".txt", dest)
shell.exec(filetxt)



# do something with it, i.e. a simple word cloud 
library(tm)
library(wordcloud)
library(Rstem)

txt <- readLines(filetxt) # don't mind warning..

#extracting each event
event <- lapply(txt, function(i) {
      j <- paste0(scan(i, what = character()), collapse = " ")
      regmatches(j, gregexpr("(?<=Call Taker).*?(?=Call Taker)", j, perl = TRUE))
})
# Write abstracts into separate txt files...

# write abstracts as txt files 
# (or use them in the list for whatever you want to do next)
lapply(1:length(event),  function(i) write.table(event[i], file = paste(event[i], "event", "txt", sep = "."), quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " " ))

# And now you're ready to do some text mining on the txt 

# originally on http://stackoverflow.com/a/21449040/1036500










txt <- tolower(txt)
txt <- removeWords(txt, c("\\f", stopwords()))

corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, removePunctuation)
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))

# Stem words
d$stem <- wordStem(row.names(d), language = "english")

# and put words to column, otherwise they would be lost when aggregating
d$word <- row.names(d)

# remove web address (very long string):
d <- d[nchar(row.names(d)) < 20, ]

# aggregate freqeuncy by word stem and
# keep first words..
agg_freq <- aggregate(freq ~ stem, data = d, sum)
agg_word <- aggregate(word ~ stem, data = d, function(x) x[1])

d <- cbind(freq = agg_freq[, 2], agg_word)

# sort by frequency
d <- d[order(d$freq, decreasing = T), ]

# print wordcloud:
wordcloud(d$word, d$freq)

# remove files
file.remove(dir(tempdir(), full.name=T)) # remove files

#################################




write.csv(txt, "txt.txt", row.names = FALSE)

mytxtfiles <- list.files(path = dest, pattern = "txt",  full.names = TRUE)
dest <-getwd()
library(tm)
mycorpus <- Corpus(DirSource(dest, pattern = "txt"))
# warnings may appear after you run the previous line, they
# can be ignored
mycorpus <- tm_map(mycorpus,  removeNumbers)
mycorpus <- tm_map(mycorpus,  removePunctuation)
mycorpus <- tm_map(mycorpus,  stripWhitespace)
mydtm <- DocumentTermMatrix(mycorpus)
# remove some OCR weirdness
# words with more than 2 consecutive characters
mydtm <- mydtm[,!grepl("(.)\\1{2,}", mydtm$dimnames$Terms)]

# get each doc as a csv with words and counts
for(i in 1:nrow(mydtm)){
      # get word counts
      counts <- as.vector(as.matrix(mydtm[1,]))
      # get words
      words <- mydtm$dimnames$Terms
      # combine into data frame
      df <- data.frame(word = words, count = counts,stringsAsFactors = FALSE)
      # exclude words with count of zero
      df <- df[df$count != 0,]
      # write to CSV with original txt filename
      write.csv(df, paste0(mydtm$dimnames$Docs[i],".csv"), row.names = FALSE) 
}





