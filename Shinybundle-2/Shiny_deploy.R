install.packages('devtools')
devtools::install_github('rstudio/rsconnect')
rsconnect::setAccountInfo(name = 'jomisilfe',
                          token = '92C0DD8F30100BB17590255422D1C8E3',
                          secret = 'COIobr9eqszqMv4IDD+XF3nq7mIC5y7x/jr2HGom')
library(rsconnect)
rsconnect::deployApp("C:/Users/jf82645/Desktop/Kaggle/Rbrockton/RBrockton")


