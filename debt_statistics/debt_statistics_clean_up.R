### Emerging Markets Debt Statistics
### Erin McDevitt

## Note: naming conventions follow BIS rules: https://www.bis.org/statistics/dsd_cbs.pdf

# Clean Session ------------------------------------------------------------

source(paste("C:/Users/", Sys.getenv("USERNAME"), "/OneDrive - Rock Creek Group/Data Bank/r_program_set_up/set_up.R", sep = ""))

## Updates Dates -----------------------------------------------------------

qtr <- "2023Q1"
year <- "2023"
quarter <- "Q1"
date <- as.Date("2023-03-01")

#Folder for Data -----------------------------------------------------------

bank <- paste("C:/Users/", Sys.getenv("USERNAME"), "/OneDrive - Rock Creek Group/Data Bank", sep = "")
data <- paste("C:/Users/", Sys.getenv("USERNAME"), "/OneDrive - Rock Creek Group/Data Bank/debt_statistics", sep = "")


setwd(paste(data, "/data", sep = ""))

#Import data ------------------------------------------------------------

## Set up - read in list of sheets and assign groups (not all sheets have groups)

bis_codes <- read_excel(paste(bank, "/documentation/bis_country_codes.xlsx", sep = "" ))

excel_file <- paste(data, "/data/global_debt_monitor_database_test.xlsx", sep = "")

# Get the list of sheet names from the Excel file - we will need to group these sheets based on similarities in structure
sheet_names <- excel_sheets(file.path(excel_file))

#Emerging and Mature Markets, tabs 2a-3b

markets <- sheet_names[grep("mature|emerging", sheet_names)]

#Sector and Currency breakdown - 5a - 8b
sector <- sheet_names[grep("debt curr", sheet_names)]

#Redemption tab 9 - 10
redemption <- sheet_names[grep("redemption", sheet_names)]

## Read in the Data

#Global Statistics and Aggregates (Tab 1)

global <- read_excel(paste(data, "/data/global_debt_monitor_database_test.xlsx", sep = ""), 
                     sheet = paste("1_global debt figures", qtr, sep = " "), skip = 4) %>%
  rename(date = "...1",
         h_p = "Households...2",
         s_p = "Non-fin corporates...3",
         o_p = "Government...4",
         f_p = "Financial Corporates...5",
         h_s = "Households...8",
         s_s = "Non-fin corporates...9",
         o_s = "Government...10",
         f_s = "Financial Corporates...11") %>%
  select(-c("...6", "...7")) %>%
  mutate(date = as.Date(as.POSIXct(date))) %>%
  pivot_longer(cols = -"date", 
               names_to = "sector", 
               values_to = "value") %>%
  mutate(rep_cty = "5J", 
         measure = toupper(gsub("_.*", "", sector)),
         instr = "D",
         curr_type = "TO1", 
         sector = toupper(gsub(".*_", "", sector)),
         cp_sector = "A")


#Global Bond Statistics
bond_markets <- read_excel(paste(data, "/data/global_debt_monitor_database_test.xlsx", sep = ""), 
                     sheet = "11_Global bond markets", skip = 3) %>%
  slice(1:(nrow(.) - 3)) %>%
  rename(date = "...1",
         `5R` = "Mature markets",
         `4T` = "Emerging markets") %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30") ) %>%
  pivot_longer(cols = -"date", 
               names_to = "rep_cty", 
               values_to = "value") %>%
  mutate(rep_cty = toupper(rep_cty), 
         measure = "S",
         instr = "D",
         curr_type = "TO1", 
         sector = "A",
         cp_sector = "A")

#State-owned enterprises 
soe <- read_excel(paste(data, "/data/global_debt_monitor_database_test.xlsx", sep = ""), 
                           sheet = "12_SOE", skip = 4) %>%
  slice(1:(nrow(.) - 2)) %>%
  rename(year = "...1") %>%
  mutate(year = as.numeric(gsub("\\(12m trailing)", " ", year)), 
         date = as.Date(paste(year, "12-31", sep = "-"))) %>%
  select(-c(year)) %>%
  pivot_longer(cols = -"date", 
               names_to = "rep_cty",
               values_to = "value") %>%
  mutate(rep_cty = toupper(rep_cty), 
         measure = "P",
         instr = "D",
         curr_type = "TO1", 
         sector = "O",
         cp_sector = "C")


#Combine global bond data and global aggregates into Rdata file 
global_aggs <- bind_rows(bond_markets, global)
global_aggs <- data.frame(global_aggs) %>% left_join(bis_codes, by = c("rep_cty" = "code"))
save(global_aggs, file = "global_aggregates.Rdata")


#Import markets (country) data

# Create an empty list to store the data frames

markets <- sheet_names[grep("mature|emerging", sheet_names)]
markets_list <- list()
new_names <- c("adv1", "adv2", "eme1", "eme2")

# Loop over each sheet and read the data into a distinct data frame - to read in every sheet takes about 1min45seconds}
for (i in seq_along(markets)) {
  # Read the sheet into a dataframe
  df <- read_excel(excel_file, sheet = markets[i])
  
  # Check if the current sheet name includes any of the substrings
  match_idx <- grep(paste(markets[i], collapse = "|"), markets)
  
  # Rename the dataframe if there is a match
  if (length(match_idx) > 0) {
    names(df) <- new_names[match_idx]
  }
  
  # Store the renamed dataframe in the new list
  markets_list[[i]] <- df
}

#original loop 
markets_list <- list()

for (sheet_name in markets) {
  
  # Read the sheet into a data frame
  sheet_data <- read_excel(paste(excel_file), sheet = sheet_name, skip = 1)
  
  # Assign the data frame to a distinct name in the list
  markets_list[[sheet_name]] <- sheet_data  %>%
    .[-1,] 
  
  rm(sheet_data)
  
}
  
  



### "Mature Markets" aka Advanced Economies 

# Step 1: Import data


  adv <-read_excel(paste(data, "/data/global_debt_monitor_database_test.xlsx", sep = ""), 
                   sheet = paste("2a_mature markets", qtr, sep = " "), skip = 1) %>% #skip "Table of contents" in first row
    .[-1,] #remove first row with empty values

## Step 2: Find and Fix columns with actual names (i.e. not `...[number]`)

  #Create list of those columns to rename - those that have "words" or descriptions in titles
  columns_for_rename <- colnames(adv)[!grepl("\\.\\.\\.\\d+$", colnames(adv)) | grepl("Aggregates\\.\\.\\.\\d+$", colnames(adv))]

  #clean up those columns to easy to read names
  new_column_names <- c("date", "hh", "nfc", "gov", "fs", "agg_pct", "agg_usd")
  matching_indices <- match(columns_for_rename, colnames(adv))
  colnames(adv)[matching_indices] <- new_column_names
  
  #Find index (starting position) for each category (column) and associated name
  column_index <- c()
  column_name <- c()

  for (name in new_column_names) {
    col_index <- match(name, colnames(adv))
    col_name <- colnames(adv)[col_index]
  
    #Append the col_index to the column_index vector
    column_index <- c(column_index, col_index)
    column_name <- c(column_name, col_name)
  
    rm(col_index, col_name)
  }

  #remove "date" values from the list - date doesn't need to be changed any more
  column_index <- column_index[column_index != 1]
  column_name <- column_name[column_name != "date"]

  
## Step 3: Rename the remaining columns (i.e. those with `...[number`)
  
    #Loop over column indices and renames columns by category (output looks something like "[new_column_name]_...3")
  
    new_index <- vector("numeric", length(column_index)) 
    
    for (i in 1:(length(column_index)-1)) {
      col_start <- column_index[i] + 1
      col_end <- column_index[i + 1] -1 
      col_name <- column_name[i]
      #performs the rename
      colnames(adv)[col_start:col_end] <- paste(col_name, colnames(adv)[col_start:col_end], sep = "_") 
      #Store the new index in the vector
      new_index[i] <- column_index[i] + 1 
    }
    
    #Rename last set of columns
    col_start <- column_index[length(column_index)] + 1
    col_end <- ncol(adv)
    col_name <- column_name[length(column_index)]
    
    colnames(adv)[col_start:col_end] <- paste(col_name, colnames(adv)[col_start:col_end], sep = "_")
    
    # Store the new index for the last set of columns
    new_index[length(column_index)] <- col_start
    
    #Rename sectors in the first row to be used in the following step
    
    substitutions <- c("Households" = "hh", 
                       "Non-fin corporates" = "nfc", 
                       "Government" = "gov",
                       "Financial Corporates" = "fs")
    
    adv <- adv %>%  mutate(across(everything(), ~str_replace_all(., substitutions)))

    
    #Add first row (country names/sector) to the column name. Again, skip the first column because that's the date
    
    colnames(adv)[2:ncol(adv)] <- paste(tolower(adv[1,-1]), colnames(adv)[2:ncol(adv)], sep = "_")
    
    #Remove any instance of "." and "_...[number]" from this column name
    colnames(adv) <- gsub("_\\.\\.\\.\\d+|\\.", "", colnames(adv))
    
    
## Step 4: Additional clean up
    
    
    adv_clean <- adv %>%
      select(where(~!all(is.na(.)))) %>%  #drop columns missing data, i.e. the divider columns in excel
      .[-1,] %>% #drop the first row with country/sector names
      select(-c(NA_agg_usd)) %>% #duplicate date that can be dropped
      rename(quarter_date = date, #original date value is quarters so rename
             date = NA_agg_pct) %>% #this is the yyyy-mm-dd date
      mutate(date = as.Date(as.numeric(date), origin = "1899-12-30") ) #convert excel date to R date
    
    
    #Reorder the date variables to front of data.frame
    
    columns <- c("quarter_date", "date")
    new_order <-  which(names(adv_clean) %in% columns)
    
    # Reorder columns based on desired positions
    adv_clean <- adv_clean[, c(new_order, setdiff(1:ncol(adv_clean), new_order))]
    
   
    
## Step 5: Final Output
    
    #Aggregate data set
    aggregates <- grep("date|_pct|_usd", colnames(adv_clean), value = TRUE)
    adv_aggregate <- adv_clean[aggregates]
    
    cols_to_drop <- grep("_pct|_usd", colnames(adv_clean), value = TRUE)
    
    
    #Country data set

    adv_country <- adv_clean[setdiff(names(adv_clean), cols_to_drop)] 
    
    adv_country_tranpose <- adv_country %>%
      mutate_at(vars(3:ncol(adv_country)), as.numeric) %>% #convert all non-date data to numeric
      pivot_longer(cols = -c("quarter_date", "date"),
                   names_to = "country_sector", 
                   values_to = "value") %>% #convert to long dataset to extract country and sector names
      mutate(country = toupper(gsub("_.*", "", country_sector)), #split the country column into country 
             sector = gsub(".*_", "", country_sector) ) %>% #and sector 
      select(-c(country_sector)) %>%
      pivot_wider(id_cols = c("quarter_date", "date", "country"),
                  values_from = "value", 
                  names_from = "sector") %>%
      mutate(region = "ADV",
             measure = "pct_gdp") %>%
      select(c(quarter_date, date, country, region, measure, hh, nfc, gov, fs))
    
    
    

