## Employment and Productivity Chartpack
## May 2023



# Update Employment Data -----------------------------------#

current_month <- "September"
current_year <- "2023"

prev_month <- "August"

source(paste("C:/Users/", Sys.getenv("USERNAME"), "/OneDrive - Rock Creek Group/Data Bank/r_program_set_up/set_up.R", sep = ""))

source(paste("C:/Users/", Sys.getenv("USERNAME"), "/OneDrive - Rock Creek Group/Data Bank/macro/code/labor_data.R", sep = ""))



list <- grep("us_",names(.GlobalEnv),value=TRUE)

rm(list = list)

data_folder <- paste("C:/Users/", Sys.getenv("USERNAME"), "/OneDrive - Rock Creek Group/Data Bank/macro/", sep = "")

macro_folder <- paste("C:/Users/", Sys.getenv("USERNAME"), "/OneDrive - Rock Creek Group/Ad Hoc/US Macroeconomic Developments/", sep = "")




# Save previous data in archive folder ---------------------#


files <- list.files(paste(macro_folder, "labor_market/output/underlying_figures", sep = ""), pattern = "\\.pdf$", full.names = TRUE)

for (file in files) {
  if (!file.exists(file)) {
  new_file_name <- paste0(macro_folder, "labor_market/output/underlying_figures/archive/", sub("\\.pdf", "", basename(file)), "_", prev_month, " ", current_year, ".pdf", sep = "" )
  file.copy(file, new_file_name)
  }else{
    print('File already exists.')
    }
}

chartpack <- list.files(paste(macro_folder, "labor_market/output/", sep = ""), pattern = "\\.pdf$", full.names = TRUE)

for (file in chartpack) {
  if (!file.exists(file)) {
  new_file_name <- paste0(macro_folder, "labor_market/output/archive/", sub("\\.pdf", "", basename(file)), "_", prev_month, " ", current_year, ".pdf", sep = "" )
  file.copy(file, new_file_name)
  }else{
    print('File already exists.')
  }
}

# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --             US Labor Market                          --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#


## US Unemployment Rate -------------------------------------

labor <- read_excel(paste(data_folder, "data/labor_data.xlsx", sep =""), sheet = 'us_labor') %>%
  mutate(date = as.Date(as.POSIXct(date))) 

unemp <- labor %>%
  pivot_longer(cols = -date, 
               names_to = "series",
               values_to = "value")

max_unrate <- unemp %>% filter(series == "unrate_value" & date == max(as.Date(date))) %>% pull(value)

average_prepandemic <- unemp %>% filter(series == "unrate_value") %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  mutate(average = mean(value)) %>%
  ungroup %>%
  filter(year == 2019) %>%
  pull(average) %>%
  unique()


# set the breaks on the x-axis
my_breaks <- seq(as.Date("2020-01-01"), max(as.Date(unemp$date)), by = "6 months")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%m") == "01", 
         paste(format(x, "%b"), "\n", format(x, "%y"), sep = ''), 
         format(x, "%b"))
})


plot_unrate <- unemp %>% filter(date > as.Date("2019-12-01")) %>%
  filter(series %in% c("unrate_value", "prime_age_unrate_value")) %>%
  ggplot(aes(x = date)) +
  geom_line(mapping = aes(y=value, color = series), linewidth = 1) +
  scale_color_manual(breaks = c('unrate_value', 'prime_age_unrate_value'),
                     values = c('unrate_value' = PEACH, 
                                'prime_age_unrate_value' = MEDIUMGREEN), 
                     labels = c('unrate_value' = 'Total, 16 years and over', 
                                'prime_age_unrate_value' = 'Prime age, 25-54 years') 
  ) +
  guides(shape = guide_legend(order = 2, nrow = 3),col = guide_legend(order = 2, nrow=3), fill = guide_legend(order = 1, nrow = 3)
         ) +
  plot_theme + 
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0,  size = 12),
        plot.subtitle = element_text(hjust = 0,   size = 10),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text( size = 10),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'),
        axis.ticks.length.y.left = unit(-2, "mm"), 
        axis.text.x = element_text(size = 10),
        axis.text.y.left = element_text(size = 10), 
        axis.text.y.right = element_blank(),
        axis.line.y.left = element_line(color = "grey30")
  ) +
    labs(title = 'Unemployment Rates',
       subtitle = 'Percent', 
       caption = paste('Source: BLS. As of ', format(max(unemp$date), "%B %Y"), ".", sep = "")
  ) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0,0)
  ) +
  scale_y_continuous(
    limits = c(0, 16),
    breaks = seq(2, 16, by = 2), 
    expand = c(0, 0)
  )

grid.draw(plot_unrate)


ggsave(paste0(macro_folder, 'labor_market/output/underlying_figures/US Unemployment Rates.pdf', sep = ''), plot_unrate, width = 11, height = 8, device = cairo_pdf)




## JOLTS ----------------------------------------------------


jolts <- read_excel(paste(data_folder, "data/labor_data.xlsx", sep =""),
                    sheet = "us_jolts") %>%
  mutate(date = as.Date(as.POSIXct(date))) %>%
  select(c(date, hires_value, openings_value, layoffs_value, quits_value)) %>%
  pivot_longer(cols = -date, 
               names_to = "series",
               values_to = "value")


# set the breaks on the x-axis
my_breaks <- seq(as.Date("2019-01-01"), max(as.Date(jolts$date)), by = "6 months")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%m") == "01", 
         paste(format(x, "%b"), "\n", format(x, "%y"), sep = ''), 
         format(x, "%b"))
})


plot_jolts <- jolts %>% filter(date > as.Date("2019-12-01")) %>%
  ggplot(aes(x = date)) +
  geom_line(mapping = aes(y=value, color = series), linewidth = 1) +
  scale_color_manual(breaks = c('openings_value', 'hires_value', 'layoffs_value', 'quits_value'),
                     values = c('openings_value' = PEACH, 
                                'hires_value' = GREENBLUE,
                                'layoffs_value' = MEDIUMGREEN,
                                'quits_value' = LIGHTGREY), 
                     labels = c('openings_value' = 'Job openings', 
                                'hires_value' = 'Hires',
                                'layoffs_value' = 'Layoffs and discharges',
                                'quits_value' = 'Quits') 
  ) +
  plot_theme + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 2, nrow=2), fill = guide_legend(order = 1, nrow = 2) 
  ) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0,  size = 12),
        plot.subtitle = element_text(hjust = 0,   size = 10),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text( size = 10),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'),
        axis.ticks.length.y.left = unit(-2, "mm"), 
        axis.text.x = element_text(size = 10),
        axis.text.y.left = element_text(size = 10), 
        axis.text.y.right = element_blank(),
        axis.line.y.left = element_line(color = "grey30")
  ) +
  labs(title = 'Job Openings and Labor Turnover',
       subtitle = 'Thousands',
       caption = paste('Source: BLS. As of ', format(max(ceiling_date(jolts$date, unit = "month") - 1), "%B %d, %Y"), ".", sep = "")
       
  ) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0,0)
  ) +
  scale_y_continuous(labels = comma,
    limits = c(0, 15500),
    breaks = seq(500, 15500, by = 2500), 
    expand = c(0, 0)
  ) 

grid.draw(plot_jolts)

ggsave(paste0(macro_folder, 'labor_market/output/underlying_figures/JOLTS.pdf', sep = ''), plot_jolts, width = 11, height = 8, device = cairo_pdf)


## Job Openings per Unemployed Person -----------------------

jolts_unlevel <-  bind_rows(jolts, unemp) %>%
  filter(series %in% c('openings_value', 'unlevel_value')) %>%
  pivot_wider(id_cols = c(date), 
              names_from = series, 
              values_from = value) %>%
  arrange(date) %>%
   fill(openings_value, .direction = "down" ) %>%
  mutate(ratio = openings_value/unlevel_value)

# set the breaks on the x-axis
my_breaks <- seq(as.Date("2019-01-01"), max(as.Date(jolts_unlevel$date)), by = "6 months")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%m") == "01", 
         paste(format(x, "%b"), "\n", format(x, "%y"), sep = ''), 
         format(x, "%b"))
})


plot_jolts_unlevel <- jolts_unlevel %>% filter(date >= as.Date("2019-12-01")) %>%
  ggplot(aes(x = date)) +
  geom_line(mapping = aes(y=ratio), color = PEACH, linewidth = 1) +
  geom_area(mapping = aes(y= ratio), fill = PEACH, linewidth = 1, alpha = 0.4) +
  plot_theme +
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 2, nrow=2), fill = guide_legend(order = 1, nrow = 2) 
  ) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0,  size = 12),
        plot.subtitle = element_text(hjust = 0,   size = 10),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text( size = 10),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'),
        axis.ticks.length.y.left = unit(-2, "mm"), 
        axis.text.x = element_text(size = 10),
        axis.text.y.left = element_text(size = 10), 
        axis.text.y.right = element_blank(),
        axis.line.y.left = element_line(color = "grey30")
  ) +
  labs(title = 'Job Openings per \nUnemployed Person',
       subtitle = 'Ratio',
       caption = paste('\n\n\n\n\nSource: BLS. As of ', format(max(jolts_unlevel$date), "%B %Y"), ".", sep = "")
       
  ) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0,0)
  ) +
  scale_y_continuous(
    limits = c(0, 3),
    breaks = seq(0.5, 3, by = 0.5), 
    expand = c(0, 0)
  ) 

grid.draw(plot_jolts_unlevel)


ggsave(paste0(macro_folder, 'labor_market/output/underlying_figures/Job Openings per Unemployed Person.pdf', sep = ''), plot_jolts_unlevel, width = 11, height = 8, device = cairo_pdf)




## Payrolls -------------------------------------------------

#Manufacturing and Leisure/Hospitality Payrolls

payroll <- read_excel(paste(data_folder, "data/labor_data.xlsx", sep =""),
                      sheet = "us_payroll") %>%
  mutate(date = as.Date(as.POSIXct(date)))

# set the breaks on the x-axis
my_breaks <- seq(as.Date("2019-01-01"), max(as.Date(payroll$date)), by = "6 months")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%m") == "01", 
         paste(format(x, "%b"), "\n", format(x, "%y"), sep = ''), 
         format(x, "%b"))
})

plot_payroll <- payroll %>% 
  select(c(date, total_nonfarm_change_value_MoM, total_nonfarm_roll3mo_avg_change_value_MoM)) %>% 
  filter(date > as.Date("2020-12-01")) %>%
  ggplot(aes(x=date)) +
  geom_bar(mapping = aes(y=total_nonfarm_change_value_MoM, fill="One-month change"), color = "black", position = "dodge", stat = "identity", alpha = 0.6) +
  geom_line(mapping = aes(y= total_nonfarm_roll3mo_avg_change_value_MoM, color = "Three-month trailing average"), linewidth = 1) +
  scale_fill_manual(values = c("One-month change" = MEDIUMGREEN) ) +
  scale_color_manual(values = c("Three-month trailing average" = PEACH), 
                     guide = guide_legend(order = 1)) +
  plot_theme +
  guides(shape = guide_legend(order = 1, nrow = 2),col = guide_legend(order = 1, nrow=2), fill = guide_legend(order = 2, nrow = 2) 
  ) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0,  size = 12),
        plot.subtitle = element_text(hjust = 0,   size = 10),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text( size = 10),
        legend.direction = "horizontal",
        axis.ticks.length.y.left = unit(-2, "mm"), 
        axis.text.x = element_text(size = 10),
        axis.text.y.left = element_text(size = 10), 
        axis.text.y.right = element_blank(),
        axis.line.y.left = element_line(color = "grey30")
  ) +
  labs(title = 'Nonfarm Payrolls',
       subtitle = 'Ratio',
       caption = paste('Source: BLS. As of ', format(max(payroll$date), "%B %Y"), ".", sep = "")
         ) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0,0)
  ) +
  scale_y_continuous(limits = c(0, 1000),
                     breaks = seq(200,  1000, by = 200), 
                     expand = c(0, 0)
  ) 


grid.draw(plot_payroll)



 ggsave(paste0(macro_folder, 'labor_market/output/underlying_figures/US Nonfarm Payrolls.pdf', sep = ''), plot_payroll, width = 11, height = 8, device = cairo_pdf)




## labor Force Participation Rate ---------------------------

lfpr <- unemp %>% filter(series %in% c("lfpr_value", "prime_age_lfpr_value")) %>%
  pivot_wider(id_cols = date, 
              names_from = series, 
              values_from = value)

# set the breaks on the x-axis
my_breaks <- seq(as.Date("2019-01-01"), max(as.Date(lfpr$date)), by = "6 months")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%m") == "01", 
         paste(format(x, "%b"), "\n", format(x, "%y"), sep =''), 
         format(x, "%b"))
})


plot_lfpr <- lfpr %>% filter(date >= as.Date('2019-12-01')) %>%
  ggplot(aes(x=date)) +
   geom_line(mapping = aes(y=lfpr_value, color = 'Total, 16 years and over (lhs)'), linewidth = 1) +
  geom_line(mapping = aes(y=prime_age_lfpr_value - 21, color = 'Prime age, 25-54 years (rhs)'), linewidth = 1) +
  scale_color_manual(breaks = c('Total, 16 years and over (lhs)', 'Prime age, 25-54 years (rhs)'),
                     values = c('Total, 16 years and over (lhs)' = PEACH, 
                                'Prime age, 25-54 years (rhs)' = MEDIUMGREEN)
  ) +
  plot_theme +
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 2, nrow=2), fill = guide_legend(order = 1, nrow = 2)) +
  
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0,  size = 12),
        plot.subtitle = element_text(hjust = 1,   size = 10),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text( size = 10),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'),
        axis.ticks.length.y.left = unit(-2, "mm"), 
        axis.ticks.length.y.right = unit(-2, "mm"), 
        axis.text.x = element_text(size = 10),
        axis.text.y.left = element_text(size = 10), 
        axis.text.y.right = element_text(size = 10), 
        axis.line.y.left = element_line(color = "grey30"),
        axis.line.y.right = element_line(color = "grey30")
  ) +
  labs(title = 'Labor Force Participation Rate',
       subtitle = 'Percent',
       caption = paste('Source: BLS. As of ', format(max(lfpr$date), "%B %Y"), ".", sep = "")
       
  ) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0,0)
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . + 21), #, breaks = seq(58, 65, 1)),
    limits = c(58, 64),
    breaks = seq(58, 64, by = 1), 
    expand = c(0, 0)
  ) 

grid.draw(plot_lfpr)

ggsave(paste0(macro_folder, 'labor_market/output/underlying_figures/Labor Force Participation Rate.pdf', sep = ''), plot_lfpr, width = 11, height = 8, device = cairo_pdf)



#Average Hourly Earnings and Unemployment Rate ---------------------------


#avg_earnings <- payroll %>% select(c(date, avg_hr_earnings_change_MoM, avg_hr_earnings_roll3mo_avg_change_MoM))


payroll_labor <- merge(payroll, labor) 

# set the breaks on the x-axis
my_breaks <- seq(as.Date("2020-01-01"), max(as.Date(payroll_labor$date)), by = "6 months")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%m") == "01", 
         paste(format(x, "%b"), "\n", format(x, "%y"), sep = ''), 
         format(x, "%b"))
})

plot_earnings_unrate <- payroll_labor %>% 
  select(c(date, unrate_value,avg_hr_earnings_change_YoY)) %>%
  filter(date > as.Date('2019-12-01')) %>%
  ggplot(aes(x = date)) +
  geom_line(mapping = aes(y=avg_hr_earnings_change_YoY, color = 'Average Hourly Earnings, change YoY (lhs)'), linewidth = 1) +
  geom_line(mapping = aes(y=unrate_value/1.5, color = 'Unemployment Rate (rhs)'), linewidth = 1) +
  scale_color_manual(breaks = c('Average Hourly Earnings, change YoY (lhs)', 'Unemployment Rate (rhs)'),
                     values = c('Unemployment Rate (rhs)' = MEDIUMGREEN,
                                "Average Hourly Earnings, change YoY (lhs)" = PEACH)
  )  +
  plot_theme +
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 2, nrow=2), fill = guide_legend(order = 1, nrow = 2)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0,  size = 12),
        plot.subtitle = element_text(hjust = 1,   size = 10),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text( size = 10),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'),
        axis.ticks.length.y.left = unit(-2, "mm"), 
        axis.ticks.length.y.right = unit(-2, "mm"), 
        axis.text.x = element_text(size = 10),
        axis.text.y.left = element_text(size = 10), 
        axis.text.y.right = element_text(size = 10), 
        axis.line.y.left = element_line(color = "grey30"),
        axis.line.y.right = element_line(color = "grey30")
  ) +
  labs(title = 'Average Hourly Earnings \nand Unemployment Rate',
       subtitle = 'Percent',
       caption = paste('Source: BLS. As of ', format(max(payroll_labor$date), "%B %Y"), ".", sep = "")
       
  ) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0,0)
               ) +
  scale_y_continuous(sec.axis = sec_axis(~. * 1.5, breaks = seq(0, 15, 3)),
                     limits = c(0, 10),
                     breaks = seq(0,10, by = 2), 
                     expand = c(0, 0)
  ) 

grid.draw(plot_earnings_unrate)

ggsave(paste0(macro_folder, 'labor_market/output/underlying_figures/Average Earnings and Unemployment.pdf', sep = ''), plot_earnings_unrate, width = 11, height = 8, device = cairo_pdf)


# ----------------------------------------------------------#
# ----------------------------------------------------------#
# --                Weekly Claims                         --#
# ----------------------------------------------------------#
# ----------------------------------------------------------#

# ----------------------------------------------------------#
## Initial Jobless Claims ----------------------------------#
# ----------------------------------------------------------#
us_claims <- read_excel(paste(data_folder, "data/labor_data.xlsx", sep =""), sheet = 'us_claims') %>%
  mutate(date = as.Date(as.POSIXct(date))) 

last_cont <- us_claims %>% select(c(date, value_continued)) %>% filter(date == max(date)) 
avg_2019 <- us_claims %>% select(c(date, value_initial)) %>% filter(year(date) == 2019) %>% 
  summarize(average = mean(value_initial)) %>%
  pull(average) %>%
  unique()


# set the breaks on the x-axis
my_breaks <- seq(as.Date("2022-01-01"), max(as.Date(us_claims$date)), by = "6 months")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%m") == "01", 
         paste(format(x, "%b"), "\n", format(x, "%y"), sep = ''), 
         format(x, "%b"))
})

plot_claims <- us_claims %>% select(c(date,movave_3mo_initial, value_initial)) %>%filter(date >= as.Date('2022-01-01')) %>%
  ggplot(aes(x=date)) +
  geom_hline(aes(yintercept = avg_2019/1000, color = "black"), linewidth = 0.5, linetype =2) +
  geom_line(mapping = aes(y= (movave_3mo_initial/1000), color = '3-month moving average'), linewidth = 0.75) +
  geom_line(mapping = aes(y= (value_initial/1000), color = 'Weekly claims'), linetype = 2, linewidth = 0.75) +
  annotate('text', x= as.Date('2022-10-01'), y = (avg_2019/1000)+5, label = '2019 average', color = 'black', size = 3.5) +
  scale_color_manual(breaks = c('Weekly claims', '3-month moving average'),
                     values = c('3-month moving average' = PEACH,
                                'Weekly claims' = MEDIUMGREEN) ) +
  plot_theme +
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 2, nrow=2), fill = guide_legend(order = 1, nrow = 2)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0,  size = 12),
        plot.subtitle = element_text(hjust = 0,   size = 10),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.text = element_text( size = 10),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'),
        axis.ticks.length.y.left = unit(-2, "mm"), 
        axis.ticks.length.y.right = unit(-2, "mm"), 
        axis.text.x = element_text(size = 10),
        axis.text.y.left = element_text(size = 10), 
        axis.text.y.right = element_text(size = 10), 
        axis.line.y.left = element_line(color = "grey30"),
        axis.line.y.right = element_line(color = "grey30")
  ) +
  labs(title = 'Initial Jobless Claims',
       subtitle = 'Thousands',
       caption = paste('Source: BEA. As of ', format(max(us_claims$date), "%B %Y"), ".", sep = "")
         ) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0.03,0)
  ) +
  scale_y_continuous(
    limits = c(150, 275),
    breaks = seq(150, 275, by = 25),
    expand = c(0, 0)
  ) 

grid.draw(plot_claims)


ggsave(paste0(macro_folder, 'labor_market/output/underlying_figures/Initial Jobless Claims.pdf', sep = ''), plot_claims, width = 11, height = 8, device = cairo_pdf)

# ----------------------------------------------------------#
## Weekly and Initial Jobless Claims -----------------------#
# ----------------------------------------------------------#


# set the breaks on the x-axis
my_breaks <- seq(as.Date("2022-01-01"), max(as.Date(us_claims$date)), by = "3 months")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%m") == "01", 
         paste(format(x, "%b"), "\n", format(x, "%y"), sep = ''), 
         format(x, "%b"))
})

plot_claims_change <- us_claims %>% filter(date >= as.Date('2022-01-01')) %>%
  ggplot(aes(x=date)) +
  geom_bar(mapping = aes(y= (value_initial/1000)*7, fill = 'Initial Claims (rhs)'), color = "grey30", stat = "identity", position = "dodge", alpha = 0.7) +
  geom_line(mapping = aes(y= (value_continued/1000), color = 'Continued claims (lhs)'), linewidth = 0.75) +
  scale_color_manual(values = c(PEACH) ) +
  scale_fill_manual(values = c(MEDIUMGREEN)) +
  plot_theme +
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 2, nrow=2), fill = guide_legend(order = 1, nrow = 2)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title = element_text(hjust = 0,  size = 12),
        plot.subtitle = element_text(hjust = 1,   size = 10),
        plot.caption = element_text(hjust = 0,  size = 10, color = "grey30"),
        legend.justification = "center",
        legend.text = element_text( size = 10),
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, 'cm'),
        axis.ticks.length.y.left = unit(-2, "mm"), 
        axis.ticks.length.y.right = unit(-2, "mm"), 
        axis.text.x = element_text(size = 10),
        axis.text.y.left = element_text(size = 10), 
        axis.text.y.right = element_text(size = 10), 
        axis.line.y.left = element_line(color = "grey30"),
        axis.line.y.right = element_line(color = "grey30")
  ) +
  labs(title = 'Weekly Jobless Claims',
       subtitle = 'Thousands',
       caption = paste('Source: BEA. As of ', format(max(us_claims$date), "%B %Y"), ".", sep = "")
         ) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0,0)
  ) +
  scale_y_continuous(labels = comma,
    sec.axis = sec_axis(~. / 7), #, breaks = seq(150, 300, 50)),
    limits = c(0, 2100),
    breaks = seq(0, 2100, 300),
    expand = c(0, 0)
  ) 

grid.draw(plot_claims_change)


ggsave(paste0(macro_folder, 'labor_market/output/underlying_figures/Weekly Jobless claims.pdf', sep = ''), plot_claims_change, width = 11, height = 8, device = cairo_pdf)


# Export

title <- ggdraw() + draw_label("Employment Conditions", fontface='bold')
caption <- ggdraw() + draw_label(paste0("RockCreek - ", format(Sys.Date(), "%B %d, %Y"), sep = ''), size = 8,
                                 fontface = 'plain', x=0.03, y = 0.3, hjust = 0, alpha = 0.5)

mc1 <- list(plot_unrate, plot_earnings_unrate, plot_payroll, plot_lfpr)
mc2 <- list(plot_jolts, plot_jolts_unlevel, plot_claims, plot_claims_change)

page1 <- plot_grid(mc1[[1]], mc1[[2]], mc1[[3]], mc1[[4]],
                        align = 'hv', nrow = 2, ncol = 2)


pdf(paste0(macro_folder,'labor_market/output/underlying_figures/page1.pdf', sep = ''), width = 8.27, height = 11.69)
plot_grid(title, page1, caption, align = 'hv', nrow = 3, ncol = 1, rel_heights=c(0.1, 1, 0.05)) 
dev.off()


page2 <- plot_grid(mc2[[1]], mc2[[2]], mc2[[3]], mc2[[4]],
                   align = 'hv', axis = "b", nrow = 2, ncol = 2)

  
pdf(paste0(macro_folder,'labor_market/output/underlying_figures/page2.pdf', sep = ''), width = 8.27, height = 11.69)
plot_grid(title, page2, caption, align = 'hv', nrow = 3, ncol = 1, rel_heights=c(0.1, 1, 0.05)) 
dev.off()


input_path <- paste0(macro_folder,'labor_market/output/underlying_figures', sep = '')

output_path <- paste0(macro_folder,'labor_market/output/Employment Conditions.pdf', sep = '')

setwd(input_path)
staple_pdf(input_directory = NULL, input_files = c("page1.pdf", "page2.pdf"), output = output_path, overwrite = TRUE)


