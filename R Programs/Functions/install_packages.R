##
## Program for first time users to install relevant R packages

## General Tools------------------------------------------------

options(warn=-1)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos = 'http://cran.us.r-project.org')
  sapply(pkg, require, character.only = TRUE)
}
ipak(c('tidyverse', 'zoo', 'patchwork', 'devtools', 'staplr',
       'gridExtra', 'cowplot', 'writexl', 'openxlsx', 'kableExtra', 'ggforce',
       'date', 'ggtext', 'ragg', "timeDate"))


#List of explanations for packages
# tidyverse: general toolkit
# zoo: irregular time series data
# patchwork: combine plots 
# devtools: updates to libraries
# staplr: combine plot outputs to one PDF 
# gridExtra: layout multiple plots on one page
# cowplot: alt layout multiple plots on one page
# writexl: alt to exporting data to excel spreadsheets
# openxlsx: modify excel spreadsheets 
# kableExtra: create tables
# ggforce: annotate plots with shapes
# date: adding circle annotations to date plots
# ggtext: mixing font styles in ggplot
# ragg: fonts


## APIs --------------------------------------------------------

# The first install function listed is for the CRAN-approved version, while the second (if applicable) is for the 
# Github version with the latest development version. Usually the CRAN version suffices. 

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos = 'http://cran.us.r-project.org')
  sapply(pkg, require, character.only = TRUE)
}
ipak(c('fredr', 'wbstats', 'WDI', 'tidyquant', 'httr', 'jsonlite', 'BIS', 'Quandl', 'alphavantager'))


#Github updates

# ipak <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
#   if (length(new.pkg)) 
#     devtools::install_github(new.pkg)
#   sapply(pkg, require, character.only = TRUE)
# }
# ipak(c('expersso/OECD', 'mingjerli/IMFData'))


#httr and jsonlite: data-interchange format
#fredr: Download US FRED
#wbstats: World Bank data 
#WDI: alt World Bank data 
#tidyquant: financial analysis tool
#alpha vantage: additional financial markets data
#Quandl: Nasdaq financial data

#API alts 

#FRED: devtools::install_github('sboysel/fredr')
#World Bank: devtools::install_github('nset-ornl/wbstats')
#IMF: install.packages('IMFData')
#BIS: devtools::install_github('expersso/BIS') 
#Tidyquant: devtools::install_github('business-science/tidyquant')
#NASDAQ: devtools::install_github('quandl/quandl-r')
#alpha vantage: devtools::install_github('business-science/alphavantager')







