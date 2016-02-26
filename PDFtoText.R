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
txt <- str_c(text, collapse = "\n")

txtparts <- unlist(str_split(txt, '                [[:digit:]]{4}'))
cat()



