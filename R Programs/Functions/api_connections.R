### API Connections to relevant financial and economic data sources 
### Erin McDevitt
### 2/23/2023  

# ----------------------------------------------------------#

#Updating API Keys

#usethis::edit_r_environ()

#FRED Data -------------------------------------------------#

  library(fredr)
  
    #FRED APi keys
    fredr_set_key("f74f32c193c1be8e77b04e74adb20b1b")
    fredr_get_key()
    

#World Bank Data -------------------------------------------#

  library(WDI)
  #library(wbstats)
    
#OECD Data -------------------------------------------------#
    
  library(OECD)

# General Financial Data  ----------------------------------#

  library(tidyquant)
    
    #Alpha Vantage API Key 
    av_api_key("NKYOU7YGO4CKAH8C")

# Nasdaq ---------------------------------------------------#

  library(Quandl)
    
    #Quandl APi Key
    Quandl.api_key("VY9QopQZZ1FsidWFTKGM")

# Alpha Vantage --------------------------------------------#    

  library(alphavantager)
    
    #Alpha Vantage API Key 
    av_api_key("NKYOU7YGO4CKAH8C")

# BIS ------------------------------------------------------#
    
  library(BIS)
    
    
    
