# download pdftotxt from 
# ftp://ftp.foolabs.com/pub/xpdf/xpdfbin-win-3.03.zip
# and extract to program files folder

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


text <- readLines(filetxt)

#extracting each event

#parsing text
text <- readLines("testpdf_pdfbox.txt")

#marking where to split

for (i in seq_along(text)){ 
      
     if (str_detect(text[i],'                [[:digit:]]{4}')) { 
           text[i] <- paste0("CALL BEGINS HERE",text[i] )
     }

}


txt <- str_c(text, collapse = "\n")

#split the text by calls
txtparts <- unlist(str_split(txt, "CALL BEGINS HERE"))

#trim leading and trailing white spaces

time <- str_trim(str_extract(txtparts, "                [[:digit:]]{4}"))
date <- str_extract(txtparts[1], "\\d{2}/\\d{2}/\\d{4}")
Call_taker <- str_extract(txtparts, "Call Taker:.*\n")
address <- str_extract(txtparts, "Location/Address:.*\n")





