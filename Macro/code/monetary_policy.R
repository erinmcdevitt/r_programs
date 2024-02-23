### Macro Data Pull - Monetary Policy - MAYBE RENAME TO CENTRAL BANK DATA???
### Erin McDevitt
### 2/23/2023

### This program pulls all rate-related data, including:
# 1) Fed Funds Rate 
# 2) Foreign CB Interest Rates 
# 3) Mortgage Rates
# 4) Treasuries 
# 5) Central Bank Balance Sheets



#use this to search series

# popular_series <- fredr_series_search_text(
#   search_text = "ICSA",
#   order_by = "popularity",
#   sort_order = "desc"
# )
# 
# View(popular_series)
#rm(popular_series)


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos = 'http://cran.us.r-project.org')
  sapply(pkg, require, character.only = TRUE)
}
ipak(c('ecb'))

library(ecb)


if (!file.exists(paste(macro_data, "data/monetary_policy_data.xlsx", sep =""))) {
  mp <- createWorkbook()
  saveWorkbook(mp, file = paste(macro_data, "data/monetary_policy_data.xlsx", sep = ""), overwrite = FALSE)
  print('New file created.')
}else{
  print('File already exists.')
}

mp <- loadWorkbook(paste(macro_data, "data/monetary_policy_data.xlsx", sep = ""), isUnzipped = FALSE)

current_fomc <- paste("September 2023")
current_fomc_date <- as.Date("2023-09-30")

# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --                  Interest Rates                      --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#


#US, ECB, BOE, BOC, and BOJ

list <- list(
  series_id = c("EFFR", "DFEDTARU", "ECBDFR", "IRSTCB01CAM156N"),
  frequency = c("m")
)


interest_rates <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x,   frequency = .y, aggregation_method = c("eop"))) %>%
  select(c(date, series_id, value))  %>%
  filter(date >= as.Date("2000-01-01")) %>%
  arrange(date) %>%
  mutate(desc = case_when(series_id == "EFFR" ~ "us_fedfunds",
                          series_id == "DFEDTARU" ~ "us_fed_target",
                          series_id == "ECBDFR" ~ "ecb_rate",
                          series_id == "IRSTCB01CAM156N" ~ "boc_rate",))
#CB data from Quandl

interest_rates_other <- tq_get("BOE/IUDBEDR", get = "quandl", from = "2000-01-01", collapse = "monthly") %>%
  mutate(date = floor_date(date, "month"), 
    desc = "uk_rate") %>% select(-c(symbol)) 


interest_rates_export <- interest_rates %>% bind_rows(interest_rates_other) %>%
  arrange(date) %>%
  pivot_wider(id_cols = date, 
              values_from = value, 
              names_from = desc) %>%
  mutate(boc_rate = round(boc_rate, digits = 4)) %>%
  fill(c(ecb_rate:us_fed_target), .direction = "down")

#Update data in the monetary policy workbook

if (!('interest_rates' %in% names(mp))) {
  addWorksheet(mp, 'interest_rates') 
  print('Worksheet added')
}else{
  print('Worsheet already exists')
}

writeData(mp, sheet = "interest_rates", interest_rates_export, colNames = T)
saveWorkbook(mp,"data/monetary_policy_data.xlsx",overwrite = T)

#removeWorksheet(mp, "interest_rates")

#Fed DOT Plot

# Dot Plots are only available from the most recent submission of projections, 
# which are submitted 4 times a year (every other FOMC meeting), so this 
# update adds on to previous projections

us_dot_timeseries <- read_excel(paste0(macro_data, "/data/monetary_policy_data.xlsx", sep = ""), sheet = "us_dot_plot") %>%
  mutate(projection_date = as.Date(as.POSIXct(projection_date)), 
         date = as.Date(as.POSIXct(date))) %>%
  filter(fomc_meeting_date != current_fomc )

list <- list(
  series_id = c("FEDTARMD"),
  frequency = c("a")
)

fed_dot_update <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  mutate(desc = case_when(series_id == "FEDTARMD" ~ "median_dot"),
         fomc_meeting_date = paste0(current_fomc), 
         year = year(date),
         date = current_fomc_date,
         projection_date = as.Date(paste0("12/31/", year, sep = ""), format = "%m/%d/%Y") ) %>%
  filter(year > 2022)

#reshape for export
fed_dot_update_export <- fed_dot_update %>% pivot_wider(
  id_cols = c(date, fomc_meeting_date, projection_date), 
  names_from = desc, 
  values_from = value) %>%
  arrange(desc(date)) #most recent data at top

#merge with time series data 

fed_dot_update_export <- bind_rows(us_dot_timeseries, fed_dot_update_export) %>% unique()

#Update data in the monetary policy workbook

if (!('us_dot_plot' %in% names(mp))) {
  addWorksheet(mp, 'us_dot_plot') 
  print('Worksheet added')
}else{
  print('Worsheet already exists')
}

writeData(mp, sheet = "us_dot_plot", fed_dot_update_export, colNames = T)
saveWorkbook(mp,paste(macro_data, "/data/monetary_policy_data.xlsx", sep = ""),overwrite = T)


# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --                  Mortgage Rates                      --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#

list <- list(
  series_id = c("MORTGAGE30US"),
  frequency = c("w")
)

mortgage_rates <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value))  %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "MORTGAGE30US" ~ "us_mortgage_rates"))

mortgage_rates_export <- mortgage_rates %>%
  pivot_wider(id_cols = date, 
              values_from = value, 
              names_from = desc)

#Update data in the monetary policy workbook

if (!('mortgage_rates' %in% names(mp))) {
  addWorksheet(mp, 'mortgage_rates') 
  print('Worksheet added')
}else{
  print('Worsheet already exists')
}

writeData(mp, sheet = "mortgage_rates", mortgage_rates_export, colNames = T)
saveWorkbook(mp,"data/monetary_policy_data.xlsx",overwrite = T)

# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --               CB Balance Sheets                      --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#



list <- list(
  series_id = c("WALCL", "TOTBORR", "RESPPANWW", "TOTBORR", "WLODLL", "WLCFLPCL",
                "WOFSRBRBC"),
  frequency = c("w")
)

fed_assets <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  arrange(series_id, date) %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "WALCL" ~ "total_assets",
                          series_id == "WOFSRBRBC" ~ "total_rb_credit"
                          series_id == "TOTBORR" ~ "total_borrowing",
                          series_id == "RESPPANWW" ~ "total_asset_noadjust", 
                          series_id == "WLODLL" ~ "total_deposits", 
                          series_id == "WLCFLPCL" ~ "asset_primary_credit")) %>%
  unique()


ecb_assets <- get_data("BSI.M.4F.N.N.T00.A.1.Z5.0000.Z01.E", filter = list(startPeriod = "2000")) 

ecb_gdp <- get_data("MNA.Q.Y.I8.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.V.N", filter = list(startPeriod = "2000")) 



list <- list(
  series_id = c("RESPPANWW", "ECBASSETSW"),
  frequency = c("w")
)

cb_tot_asset <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value))  %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "RESPPANWW" ~ "fed_total_assets",
                          series_id == "ECBASSETSW" ~ "ecb_total_assets")) %>%
  arrange(date) %>%
  mutate(year = year(date), 
         month = month(date), 
         day = day(date)) %>%
  arrange(year, month, day) %>%
  group_by(series_id, year, month) %>%
    mutate(max_date = case_when(date == max(date) ~ date, 
                                TRUE ~ NA), 
           last_value = case_when(date == max(date) ~ value, 
                                  TRUE ~ NA) ) %>%
  ungroup() %>%
  filter(!is.na(last_value)) %>%
  mutate(date = floor_date(date, "month")) %>%
  select(c(date, desc, last_value)) %>%
  pivot_wider(id_cols = "date",
              names_from = "desc", 
              values_from ="last_value") %>%
  mutate(fed_total_assets = fed_total_assets/1000)

list <- list(
  series_id = c("GDP", "ECBASSETSW"), #not the right GDP value I dont think
  frequency = c("q")
)


gdp_cb <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value))  %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "GDP" ~ "us_gdp",
                          series_id == "ECBASSETSW" ~ "euro_gdp")) %>%
  pivot_wider(id_cols = "date",
              names_from = "desc", 
              values_from ="value")

cb_gdp <- gdp_cb %>% left_join(cb_tot_asset) %>% mutate(fed_gdp  = fed_total_assets/us_gdp * 100, 
                                                             ecb_gdp = ecb_total_assets/euro_gdp * 100) %>%
  arrange(desc(date))
  

# 11/30/2022	 34 	 64 	 128 	 39 
# 10/31/2022	 35 	 67 	 126 	 39 
# 9/30/2022	 35 	 68 	 124 	 38 
# 8/31/2022	 35 	 67 	 128 	 39 
# 7/31/2022	 36 	 68 	 131 	 39 
# 6/30/2022	 36 	 69 	 133 	 40 
# 5/31/2022	 37 	 69 	 134 	 40 
# 4/30/2022	 37 	 69 	 134 	 40 



# ----------------------------------------------------------#
## Check if Sheet 1 is a thing and if so, delete

if (("Sheet 1" %in% names(mp))) {
  removeWorksheet(mp, "Sheet 1") 
  print('Worksheet deleted.')
}else{
  print("Worsheet doesn't exist.")
}

saveWorkbook(mp, file = "data/monetary_policy_data.xlsx", overwrite = TRUE)

