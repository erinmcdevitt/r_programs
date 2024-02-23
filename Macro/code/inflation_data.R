### Macro Data Pull - Prices/Inflation
### Erin McDevitt
### 3/7/2023

### This program pulls all prices-related data, including:
# 1) CPI
# 2) PCE
# 3) International Prices


# Most of this data is from FRED, so the search function below helps
# fine series

# popular_series <- fredr_series_search_text(
#   search_text = "ICSA",
#   order_by = "popularity",
#   sort_order = "desc"
# )
# 
# View(popular_series)
#rm(popular_series)


macro_data <- paste("C:/Users/", Sys.getenv("USERNAME"), sep = "")


if (!file.exists(paste(macro_data, "/data/inflation_data.xlsx", sep = ""))) {
  pd <- createWorkbook()
  saveWorkbook(pd, file = paste0(macro_data, "/data/inflation_data.xlsx"), overwrite = TRUE)
  print('New file created.')
}else{
  print('File already exists.')
}

pd <- loadWorkbook(paste0(macro_data, "/data/inflation_data.xlsx"))

# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --                        CPI                           --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#


# ----------------------------------------------------------#
## US CPI --------------------------------------------------#
# ----------------------------------------------------------#

# Headline CPI - incl. food and energy breakouts -----------#

list <- list(
  series_id = c("CPIAUCSL", "CPIAUCNS", "CPIUFDSL", "CPIUFDNS", 
                "CPIENGSL", "CPIENGNS"),
  frequency = c("m")
)

uscpi_head <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(change_MoM = ((value - lag(value))/ lag(value)) * 100, 
         change_YoY = (value - lag(value, 12))/ lag(value, 12) * 100) %>%
  ungroup() %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "CPIAUCSL" ~ "cpi_headline_sa", 
                          series_id == "CPIAUCNS" ~ "cpi_headline_nsa",
                          series_id == "CPIUFDSL" ~ "cpi_food_sa",
                          series_id == "CPIUFDNS" ~ "cpi_food_nsa",
                          series_id == "CPIENGSL" ~ "cpi_energy_sa",
                          series_id == "CPIENGNS" ~ "cpi_energy_nsa"))


# Core CPI --------------------------------------------------#

list <- list(
  series_id = c("CPILFESL", "CPILFENS", "CUSR0000SACL1E", "CUUR0000SACL1E", 
                "CUSR0000SASLE", "CUUR0000SASLE"),
  frequency = c("m")
)


uscpi_core <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(change_MoM = ((value - lag(value))/ lag(value)) * 100, 
         change_YoY = (value - lag(value, 12))/ lag(value, 12) * 100) %>%
  ungroup() %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "CPILFESL" ~ "cpi_core_sa", 
                          series_id == "CPILFENS" ~ "cpi_core_nsa",
                          series_id == "CUSR0000SACL1E" ~ "cpi_goods_sa",
                          series_id == "CUUR0000SACL1E" ~ "cpi_goods_nsa",
                          series_id == "CUSR0000SASLE" ~ "cpi_services_sa",
                          series_id == "CUUR0000SASLE" ~ "cpi_services_nsa"))


rm(list)

uscpi_export <- bind_rows(uscpi_head, uscpi_core) %>%
  pivot_wider(id_cols = date, 
              names_from = desc, 
              values_from = c(value, change_MoM, change_YoY),
              names_glue = "{desc}_{.value}") %>%
  arrange(desc(date))


#Update data in the prices workbook
if (!('us_cpi' %in% names(pd))) {
  addWorksheet(pd, 'us_cpi') 
  print("Sheet created.")
}else{
  print('Sheet already exists.')
}


writeData(pd, sheet = "us_cpi", uscpi_export, colNames = T)
saveWorkbook(pd, paste0(macro_data,"data/inflation_data.xlsx", sep = ""),overwrite = T)


# ----------------------------------------------------------#
## Global CPI ----------------------------------------------#
# ----------------------------------------------------------#

# Canada CPI -----------------------------------------------#


list <- list(
  series_id = c("CANCPIALLMINMEI", "CANCPICORMINMEI"),
  frequency = c("m")
)

can_cpi <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(change_MoM = ((value - lag(value))/ lag(value)) * 100, 
         change_YoY = (value - lag(value, 12))/ lag(value, 12) * 100) %>%
  ungroup() %>%
    filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "CANCPIALLMINMEI" ~ "can_cpi_head_nsa",
                          series_id == "CANCPICORMINMEI" ~ "can_cpi_core_nsa")) 



# Euro Area CPI --------------------------------------------#


list <- list(
  series_id = c("CPHPTT01EZM661N", "CPHPLA01EZM661N"),
  frequency = c("m")
)

euro_cpi <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
    arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(change_MoM = ((value - lag(value))/ lag(value)) * 100, 
         change_YoY = (value - lag(value, 12))/ lag(value, 12) * 100) %>%
  ungroup() %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "CPHPTT01EZM661N" ~ "euro_cpi_head_nsa",
                          series_id == "CPHPLA01EZM661N" ~ "euro_cpi_core_nsa")) 



# UK CPI --------------------------------------------------#


list <- list(
  series_id = c("CPHPTT01GBM659N"),
  frequency = c("m")
)

gb_cpi_head <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  mutate(change_YoY = value, 
         change_MoM = NA) %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "CPHPTT01GBM659N" ~ "gb_cpi_head_nsa")) 

rm(list)

# gb_cpi_core <- read_excel("data/Graphs for Macro Deck.xlsx",
#                         sheet = "Core CPI Across Countries", 
#                         range = "E4:I100") %>%
#   mutate(change_YoY = UK,
#          value = NA,
#          change_MoM = NA, 
#          date = floor_date(as.Date(as.POSIXct(Date)), "month")) %>%
#   filter(!is.na(change_YoY)) %>%
#   mutate(series_id = as.character(NA),
#     desc  = "gb_cpi_core_nsa") %>%
#   select(c(date, series_id, change_YoY, desc))
# 
# gb_cpi <- bind_rows(gb_cpi_head, gb_cpi_core)



# Japan CPI -----------------------------------------------#

# 
# jp_cpi <- read_excel("data/Graphs for Macro Deck.xlsx",
#                      sheet = "CPI Across Countries", 
#                      range = "E4:K100") %>%
#   mutate(jp_cpi_head_nsa_change_YoY = Japan,
#     date = floor_date(as.Date(as.POSIXct(Date)), "month")) %>%
#   filter(!is.na(jp_cpi_head_nsa_change_YoY)) %>%
#   left_join( read_excel("data/Graphs for Macro Deck.xlsx",
#                         sheet = "Core CPI Across Countries", 
#                         range = "E4:I100")) %>%
#   mutate(jp_cpi_core_nsa_change_YoY = Japan,
#          date = floor_date(as.Date(as.POSIXct(Date)), "month")) %>%
#   filter(!is.na(jp_cpi_core_nsa_change_YoY)) %>%
#   select(c(date, jp_cpi_head_nsa_change_YoY, jp_cpi_core_nsa_change_YoY)) 
#   
  

# Combine Global CPI ----------------------------------------# 

# intl_cpi <- bind_rows(can_cpi, gb_cpi, euro_cpi) %>%
# pivot_wider(id_cols = date, 
#               names_from = desc, 
#               values_from = c(value, change_MoM, change_YoY),
#               names_glue = "{desc}_{.value}") %>%
#   left_join(jp_cpi) 
# 
# 
# #Update data in the prices workbook
# if (!('intl_cpi' %in% names(pd))) {
#   addWorksheet(pd, 'intl_cpi')
#   print("Sheet created")
# }else{
#   print('Sheet already exists.')
# }
# 
# 
# writeData(pd, sheet = "intl_cpi", intl_cpi, colNames = T)
# saveWorkbook(pd, paste(macro_data, "/data/inflation_data.xlsx", sep = ""),overwrite = T)

# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --                        PCE                           --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#


#US PCE and Indices ----------------------------------------#

## US PCE and Components


list <- list(
  series_id = c("PCEPILFE", "PCEPI", "DGDSRG3M086SBEA", "DSERRG3M086SBEA",
                "PI", "W209RC1", "PMSAVE", "PSAVERT", "DPCERAM1M225NBEA"),
  frequency = c("m")
)


uspce <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  filter(date >= as.Date("1999-12-01")) %>%
    arrange(date, series_id) %>%
  group_by(series_id) %>%
  mutate(change_MoM = ((value - lag(value))/ lag(value)) * 100, 
         change_YoY = ((value - lag(value, 12))/ lag(value, 12)) * 100,
         change_3Mo = ((value - lag(value, 3))/ lag(value, 3)) * 100,
         ann_MoM = (((value/ lag(value, 1))^12) - 1) *100,
         ann_3Mo = (((value/ lag(value, 3))^4) - 1) *100,
         ann_6Mo = (((value/ lag(value, 6))^2) - 1) *100, 
         roll3mo_avg = rollmean(ann_MoM, 3, mean, na.rm = TRUE, fill = NA, align = "right"),
         roll6mo_avg = rollmean(ann_MoM, 6, mean, na.rm = TRUE, fill = NA, align = "right")) %>%
  mutate(desc = case_when(series_id == "PCEPILFE" ~ "pcei_core",
                          series_id == "PCEPI" ~ "pcei", 
                          series_id == "DGDSRG3M086SBEA" ~ "pcei_core_goods", 
                          series_id == "DSERRG3M086SBEA" ~ "pcei_core_services", 
                          series_id == "PI" ~ "pi_income",
                          series_id == "W209RC1" ~ "pi_wages", 
                          series_id == "DPCERAM1M225NBEA" ~ "pi_spending",
                          series_id == "PMSAVE" ~ "ps_savings", 
                          series_id == "PSAVERT" ~ "ps_savings_rate")) 


#rm(list)

list <- colnames(uspce)
var <- list[! list %in% c('date', 'year', 'series_id', 'desc')]

pce_export <- uspce %>% arrange(desc) %>% pivot_wider(
  id_cols = date, 
  names_from = desc, 
  values_from = c(var),
  names_glue = "{desc}_{.value}") %>%
  select(-contains(c("ps_savings_rate_change", "savings_rate_roll", "savings_rate_ann")))


#Update data in the prices workbook
if (!('us_pce' %in% names(pd))) {
  addWorksheet(pd, 'us_pce') 
  print("Sheet created")
}else{
  print('Sheet already exists.')
}


writeData(pd, sheet = "us_pce", pce_export, colNames = T)
saveWorkbook(pd, paste0(macro_data,"data/inflation_data.xlsx", sep = ""),overwrite = T)





# ----------------------------------------------------------#
## Check if Sheet 1 is a thing and if so, delete

if (("Sheet 1" %in% names(pd))) {
  removeWorksheet(pd, "Sheet 1") 
  print('Worksheet deleted.')
}else{
  print("Worsheet doesn't exist.")
}

saveWorkbook(pd, file = paste0(macro_data,"data/inflation_data.xlsx", sep = ""), overwrite = TRUE)

