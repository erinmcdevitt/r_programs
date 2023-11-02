### Macro Data Pull - Housing
### Erin McDevitt
### 2/23/2023

### This program pulls all production and business related data, including:
# 1) Housing (starts, permits, sales) 
# 2) Construction (spending)


#use this to search series

# popular_series <- fredr_series_search_text(
#   search_text = "ICSA",
#   order_by = "popularity",
#   sort_order = "desc"
# )
# 
# View(popular_series)
#rm(popular_series)

# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --                    Housing                           --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#

# House data from FRED is only available for the prior 12 months, 
# so this pull builds a data set that adds new data to the spreadsheet 
# as it comes in
# Thanks National Association of Realtors

us_housing_timeseries <- read_excel("data/housing_data.xlsx", sheet = "us_housing")

list <- list(
  series_id = c("HOUST", "PERMIT", "EXHOSLUSM495S", "HOUST1F"),
  frequency = c("m")
)

us_housing_update <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(change_MoM = ((value - lag(value))/ lag(value)) * 100, 
         change_YoY = (value - lag(value, 12))/ lag(value, 12) * 100) %>%
  ungroup() %>%
  filter(date >= as.Date(max(date))) %>%
  mutate(desc = case_when(series_id == "HOUST" ~ "housing_starts", 
                          series_id == "PERMIT" ~ "housing_permits", 
                          series_id == "HOUST1F" ~ "housing_permits_single", 
                          series_id == "EXHOSLUSM495S" ~ "housing_sales_existing"))

#reshape for export
new_us_house_for_export <- us_housing_update %>% pivot_wider(
  id_cols = date, 
  names_from = desc, 
  values_from = c(value, change_MoM, change_YoY) ) %>%
  arrange(desc(date)) #most recent data at top

#merge with time series data 

us_housing <- bind_rows(us_housing_timeseries, new_us_house_for_export) %>% unique(date)

# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --                 Construction                         --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#

list <- list(
  series_id = c("TLNRESCONS"),
  frequency = c("m")
)

us_construction <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(change_MoM = ((value - lag(value))/ lag(value)) * 100, 
         change_YoY = (value - lag(value, 12))/ lag(value, 12) * 100) %>%
  ungroup() %>%
  filter(date >= as.Date("2000-12-31")) %>%
  mutate(desc = case_when(series_id == "TLNRESCONS" ~ "nonresidential_spending"))

#reshape for export
new_construct_for_export <- us_construction %>% pivot_wider(
  id_cols = date, 
  names_from = desc, 
  values_from = c(value, change_MoM, change_YoY) ) %>%
  arrange(desc(date)) #most recent data at top

# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --                   EXPORT DATA                        --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#



write_xlsx(list(housing = data_for_export, construction = construction),
           "data/us_prod_business_activity_data.xlsx")

