### Macro Data Pull - Labor Markets
### Erin McDevitt
### 2/23/2023

### This program pulls all labor-related data, including:
# 1) Employment Situation (Current Employment Statistics)
# 2) JOLTS
# 3) Weekly Claims


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

macro_data <- paste("C:/Users/", Sys.getenv("USERNAME"), "/OneDrive - Rock Creek Group/Data Bank/macro/", sep = "")

if (!file.exists(paste(macro_data, "data/labor_data.xlsx", sep = ""))) {
  ld <- createWorkbook()
  saveWorkbook(ld, file = "data/labor_data.xlsx", overwrite = TRUE)
  print('New file created.')
}else{
  print('File already exists.')
}

ld <- loadWorkbook(paste(macro_data, "data/labor_data.xlsx", sep = ""))

# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --             Employment Situation                     --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#

#US Payrolls -----------------------------------------------#

list <- list(
  series_id = c("PAYEMS", "MANEMP", "USLAH", "CES0500000003", "USCONS"),
  frequency = c("m")
)

us_payroll <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(change_value_MoM = value - lag(value),
         change_MoM = ((value - lag(value))/ lag(value)) * 100, 
         change_YoY = (value - lag(value, 12))/ lag(value, 12) * 100,
         roll3mo_avg_change_value_MoM = rollmean(change_value_MoM, 3, mean, na.rm = TRUE, fill = NA, align = "right"),
         roll3mo_avg_change_MoM = rollmean(change_MoM, 3, mean, na.rm = TRUE, fill = NA, align = "right"),
         index = 100 * value / value[date == as.Date("2019-12-01")]) %>%
  ungroup() %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "PAYEMS" ~ "total_nonfarm", 
                          series_id == "MANEMP" ~ "manufacturing",
                          series_id == "USLAH" ~ "leisure_hospitality", 
                          series_id == "USCONS" ~ "construction",
                          series_id == "CES0500000003" ~ "avg_hr_earnings"
                          ))

us_payroll_export <- us_payroll %>% 
  pivot_wider(id_cols = date, 
              names_from = desc, 
              values_from = c(value,change_value_MoM, change_MoM, 
                              change_YoY, index, roll3mo_avg_change_value_MoM,
                              roll3mo_avg_change_MoM),
              names_glue = "{desc}_{.value}") %>%
  arrange(desc(date))


#Update data in the labor market workbook
if (!('us_payroll' %in% names(ld))) {
  addWorksheet(ld, 'us_payroll') 
}
writeData(ld, sheet = "us_payroll", us_payroll_export, colNames = T)
saveWorkbook(ld, paste(macro_data, "data/labor_data.xlsx", sep = ""),overwrite = T)

#ann_MoM = (((value/ lag(value, 1))^12) - 1) *100,
#ann_3Mo = (((value/ lag(value, 1))^4) - 1) *100,

#US Unemployment -------------------------------------------#
#US Labor Force Participation Rate -------------------------#

list <- list(
  series_id = c("UNRATE", "LNS14000060", "UNEMPLOY", "CIVPART", "LNS11300060"),
  frequency = c("m")
)

us_labor <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(change_MoM = ((value - lag(value))/ lag(value)) * 100, 
         change_YoY = (value - lag(value, 12))/ lag(value, 12) * 100,
         roll3mo_avg = rollmean(change_MoM, 3, mean, na.rm = TRUE, fill = NA, align = "right")) %>%
  ungroup() %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "UNRATE" ~ "unrate", 
                          series_id == "LNS14000060" ~ "prime_age_unrate",
                          series_id == "UNEMPLOY" ~ "unlevel",
                          series_id == "CIVPART" ~ "lfpr", 
                          series_id == "LNS11300060" ~ "prime_age_lfpr"))



us_labor_export <- us_labor %>%
  pivot_wider(id_cols = date, 
              names_from = desc, 
              values_from = c(value, change_MoM, change_YoY),
              names_glue = "{desc}_{.value}") %>%
  arrange(desc(date))

#Update data in the labor market workbook
if (!('us_labor' %in% names(ld))) {
  addWorksheet(ld, 'us_labor') 
}
writeData(ld, sheet = "us_labor", us_labor_export, colNames = T)
saveWorkbook(ld, paste(macro_data, "data/labor_data.xlsx", sep = ""),overwrite = T)

# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --                       JOLTS                          --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#


list <- list(
  series_id = c("JTSJOL", "JTSQUL", "JTSHIL", "JTSLDL", "JTSTSL"),
  frequency = c("m")
)

us_jolts <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  arrange(series_id, date) %>%
  mutate(year = year(date)) %>%
  group_by(series_id) %>%
  mutate(change_MoM = ((value - lag(value))/ lag(value)) * 100, 
         change_YoY = (value - lag(value, 12))/ lag(value, 12) * 100) %>%
  ungroup() %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "JTSJOL" ~ "openings", 
                          series_id == "JTSQUL" ~ "quits",
                          series_id == "JTSHIL" ~ "hires",
                          series_id == "JTSLDL" ~ "layoffs", 
                          series_id == "JTSTSL" ~ "separations"
                          ))



us_jolts_export <- us_jolts %>%
  pivot_wider(id_cols = date, 
              names_from = desc, 
              values_from = c(value, change_MoM, change_YoY),
              names_glue = "{desc}_{.value}") %>%
  arrange(desc(date))


#JOLTS Rates

list <- list(
  series_id = c("JTSJOR", "JTSQUR", "JTSTSR"),
  frequency = c("m")
)

us_jolts_rates <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  mutate(desc = case_when(series_id == "JTSJOR" ~ "opening_rate", 
                          series_id == "JTSQUR" ~ "quit_rate",
                          series_id == "JTSTSR" ~ "sep_rate"
  )) %>%
  pivot_wider(id_cols = date, 
              names_from = desc, 
              values_from = c(value),
              names_glue = "{desc}_{.value}") %>%
  arrange(desc(date))


us_jolts_export <- us_jolts_export %>% left_join(us_jolts_rates)



#Update data in the labor market workbook
if (!('us_jolts' %in% names(ld))) {
  addWorksheet(ld, 'us_jolts') 
}
writeData(ld, sheet = "us_jolts", us_jolts_export, colNames = T)
saveWorkbook(ld, paste(macro_data, "data/labor_data.xlsx", sep = ""),overwrite = T)


# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --                  Weekly Claims                       --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#


#Initial and Continued claims ------------------------------#

list <- list(
  series_id = c("ICSA", "CCSA"),
  frequency = c("w")
)

us_claims <- pmap_dfr(
  .l = list,
  .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  mutate(year = year(date)) %>%
    arrange(series_id, date) %>%
  group_by(series_id) %>%
  mutate(change_MoM = ((value - lag(value))/ lag(value)) * 100, 
         change_YoY = (value - lag(value, 12))/ lag(value, 12) * 100,
         movave_3mo = runMean(value, 3)) %>%
  group_by(series_id, year) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  
  ungroup() %>%
  filter(date >= as.Date("2000-01-01")) %>%
  mutate(desc = case_when(series_id == "ICSA" ~ "initial", 
                          series_id == "CCSA" ~ "continued" ))

us_claims_export <- us_claims %>%
  pivot_wider(id_cols = date, 
              names_from = desc, 
              values_from = c(value, change_MoM, change_YoY, movave_3mo)) %>%
  arrange(desc(date))


#Update data in the labor market workbook
if (!('us_claims' %in% names(ld))) {
  addWorksheet(ld, 'us_claims') 
}
writeData(ld, sheet = "us_claims", us_claims_export, colNames = T)
saveWorkbook(ld, paste(macro_data, "data/labor_data.xlsx", sep = ""),overwrite = T)



# ----------------------------------------------------------#
# ----------------------------------------------------------#

# ----------------------------------------------------------#
# ----------------------------------------------------------#


# ----------------------------------------------------------#
## Check if Sheet 1 is a thing and if so, delete

if (('Sheet 1' %in% names(ld))) {
  removeWorksheet(ld, "Sheet 1") 
  print('Worksheet deleted.')
}else{
  print("Worsheet doesn't exist.")
}

print("Update Complete")

