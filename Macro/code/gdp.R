### Macro Data Pull - Main Macroeconomic Indicators
### Erin McDevitt
### 03/03/2023

### This program pulls all major macroeconomic indicators  data, including:
# 1) GDP
# 2) 
rm(list=ls())

# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --                        GDP                           --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#


#US Real GDP Growth ----------------------------------------#

#need estimate 

list <- list(
  series_id = c("A191RL1A225NBEA"),
  frequency = c("a")
)

us_gdp <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  arrange(series_id, date) %>%
  mutate(desc = case_when(series_id == "A191RL1A225NBEA" ~ "us_gdp_act_growth"))

#reshape for export
us_gdp_for_export <- us_gdp %>% pivot_wider(
  id_cols = date, 
  names_from = desc, 
  values_from = c(value) ) %>%
  arrange(desc(date)) #most recent data at top


#International Real GDP Growth -----------------------------#

#Global GDP Growth Estimates
gdp <- read_excel("data/gdp.xlsx")


#PMI





#World Bank Data

# gdp_data <- WDIsearch('gdp')[1:10,]
# 
# gdp <- WDI(indicator='6.0.GDP_growth', country=c('MX','CA','US'), start=2010, end=2025)
# 
# #http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/<dataset identifier>/<filter expression>/<agency name>[?<additional parameters>]
# 
# #OECD Data
# 
# 
# dataset_list <- OECD::get_datasets()
# dataset <- OECD::search_dataset("outlook", data = dataset_list)
# 
# view(dataset)
# 
# dataset <- "EO112_INTERNET"
# 
# dstruc <- OECD::get_data_structure(dataset)
# str(dstruc, max.level = 1)
# 
# dstruc$VAR_DESC
# var <- dstruc$VARIABLE
# dstruc$SUBJECT
# dstruc$LOCATION
# 
# #Quarterly National Accounts
# 
# filter_list <- list("CHN", "GDPV_ANNPCT")
# 
# 
# df <- get_dataset(dataset, filter = filter_list)
# head(df)
# 
# 
# 
# df <- get_dataset("QNA", filter = list(c("CHN"), 
#                                                c("B1_GE")), 
#                   start_time = "2020-Q4", end_time = "2020-Q4")
# 
# all_ids_data_from_api <- 
#   ids_tibble %>%
#   # create API URL
#   mutate(api_url = glue("http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/EO112_INTERNET/")) %>%
#   # use httr::GET to request the data from the API
#   mutate(data_from_api = map(.x = api_url, .f = GET))








