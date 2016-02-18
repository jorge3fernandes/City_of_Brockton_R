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

txt <- readLines(filetxt) # don't mind warning..

#extracting each event
event <- lapply(txt, function(i) {
      j <- paste0(scan(i, what = character()), collapse = " ")
      regmatches(j, gregexpr("(?<=Call Taker).*?(?=Call Taker)", j, perl = TRUE))
})
# Write abstracts into separate txt files...

# write abstracts as txt files 
# (or use them in the list for whatever you want to do next)
lapply(1:length(event),  function(i) write.table(event[i], file = paste(event[i], "event", "txt", sep = "."),
                                                 quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " " ))