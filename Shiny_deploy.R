install.packages('devtools')
devtools::install_github('rstudio/rsconnect')
rsconnect::setAccountInfo(name = 'jomisilfe',
                          token = '92C0DD8F30100BB17590255422D1C8E3',
                          secret = '--')
library(rsconnect)
rsconnect::deployApp("C:/Users/jf82645/Desktop/Kaggle/Rbrockton/RBrockton")
