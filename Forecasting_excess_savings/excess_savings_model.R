
## Forecasting Excess Savings

# Flow = DPI - PCE - Other outlays
# DPI is personal income less taxes, and "Other outlays" includes (non-mortgage) interest and transfer payments by households

#Excess savings = diff in trend between predicted and observed components

list <- list(
  series_id = c("PMSAVE", "DSPI", "PCE", "A068RC1", "B069RC1"),
  frequency = c("m")
)


#Savings and Components
savings <- pmap_dfr(
    .l = list,
    .f = ~ fredr(series_id = .x, frequency = .y)) %>%
  select(c(date, series_id, value)) %>%
  filter(date >= as.Date("2013-12-01")) %>%
  arrange(date, series_id) %>%
  pivot_wider(id_cols = date, 
              names_from = series_id, 
              values_from = value) 

#Modelling and Trends using last 48 months
pre_pandemic <- savings %>% filter(date >= as.Date("2018-01-01") & date <= as.Date("2020-01-01"))


#DSPI 

dspi_model <- lm(DSPI ~ date, data = pre_pandemic)

dspi_trend <- predict(dspi_model, newdata = pre_pandemic)

predicted <- data.frame(date = seq(as.Date("2020-02-01"), max(savings$date), by = "month"))
forecast <- forecast(dspi_model, newdata = predicted)

autoplot(forecast)

dspi_df <- sapply(mget(ls(pattern = '^forecast$')), function(x) x$mean)

dspi_forecast <- data.frame(predicted$date, dspi_df) %>%
  rename(date = predicted.date)

rm(forecast, dspi_df)



dspi_trend <- savings %>% left_join(dspi_forecast, by = "date") %>%
  rename(observed = DSPI,
         predicted = forecast) %>%
  select(c(date, observed, predicted)) %>%
  mutate(excess_income = observed - predicted)


# set the breaks on the x-axis
my_breaks <- seq(min(as.Date(dspi_trend$date)), max(as.Date(dspi_trend$date)), by = "2 years")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%Y") == "2010", 
         paste(format(x, "%Y")), 
         format(x, "%Y"))
})



plot_savings <- dspi_trend %>%
  ggplot(aes(x = date)) +
  geom_line(mapping = aes(y = predicted), color = DARKGREEN, linewidth = 1, linetype = 3) +
  geom_line(mapping = aes(y = observed), color = DARKGREEN, linewidth = 1) +
  plot_theme +
  theme(legend.position = c(0.6, 0.8)) +
  labs(subtitle = 'USD Bil',
       caption = paste('\nSource: RockCreek, Bureau of Economic Analysis.')) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0,0)
  ) +  
  scale_y_continuous(labels = scales::comma,
                     sec.axis = dup_axis(),
                     # limits = c(0, 500),
                     # breaks = seq(0, 500, by = 50),
                     expand = c(0,0)

  )

grid.draw(plot_savings)


# Calculating Excess Savings

#Excess savings calc as difference in actual de-annualized personal savings and 
# trend implied by trend for the 48 months leading up to the first month fo the 2020 recession

pre_pandemic <- savings %>% filter(date >= as.Date("2018-01-01") & date <= as.Date("2020-01-01"))

# Fit a linear model to the data
model <- lm(PMSAVE ~ date, data = pre_pandemic)

# Calculate the trend line
trend <- predict(model, newdata = pre_pandemic)

ggplot(pre_pandemic, aes(date, PMSAVE)) +
  geom_point() +
  geom_line(aes(date, trend), color = "red") +
  labs(title = "Personal Savings Trend", x = "Date", y = "Personal Savings")


#Forecast one year out
predicted <- data.frame(date = seq(as.Date("2020-02-01"), max(savings$date), by = "month"))
forecast <- forecast(model, newdata = predicted)

autoplot(forecast)

forecast_df <- sapply(mget(ls(pattern = '^forecast$')), function(x) x$mean)

forecast_final <- data.frame(predicted$date, forecast_df) %>%
  rename(date = predicted.date)



savings_trend <-savings %>% filter(date > as.Date("2013-01-01")) %>% left_join(forecast_final, by = "date") %>%
  rename(observed = savings,
         predicted = forecast) %>%
  select(c(date, observed, predicted, chg)) %>%
  mutate(excess_savings = observed - predicted)


# set the breaks on the x-axis
my_breaks <- seq(min(as.Date(savings_trend$date)), max(as.Date(savings_trend$date)), by = "2 years")

# generate labels programmatically based on breaks
my_labels <- lapply(my_breaks, function(x) {
  ifelse(format(x, "%Y") == "2010", 
         paste(format(x, "%Y")), 
         format(x, "%Y"))
})



plot_savings <- savings_trend %>%
  ggplot(aes(x = date)) +
  geom_line(mapping = aes(y = predicted), color = DARKGREEN, linewidth = 1, linetype = 3) +
  geom_line(mapping = aes(y = excess_savings), color = DARKGREEN, linewidth = 1) +
  plot_theme +
  theme(legend.position = c(0.6, 0.8)) +
  labs(subtitle = 'USD Bil',
       caption = paste('\nSource: RockCreek, Bureau of Economic Analysis.')) +
  scale_x_date(breaks = my_breaks, labels = my_labels, expand = c(0,0)
  ) +
  scale_y_continuous(labels = scales::comma, 
                     sec.axis = dup_axis(),
                     limits = c(0, 7000),
                     breaks = seq(0, 7000, by = 1000),
                     expand = c(0,0)
                     
  )


