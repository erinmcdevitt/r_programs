# Fit a linear model to the data
model <- lm(savings ~ date, data = pre_pandemic)

# Calculate the trend line
trend <- predict(model, newdata = pre_pandemic)

ggplot(pre_pandemic, aes(date, savings)) +
  geom_line() +
  geom_line(aes(date, trend), color = "red") +
  labs(title = "Personal Savings Trend", x = "Date", y = "Personal Savings")


#Forecast one year out
predicted <- data.frame(date = seq(as.Date("2020-02-01"), max(savings$date), by = "month"))
forecast <- forecast(model, newdata = predicted)

autoplot(forecast)

forecast_df <- sapply(mget(ls(pattern = '^forecast$')), function(x) x$mean)

forecast_final <- data.frame(predicted$date, forecast_df) %>%
  rename(date = predicted.date)

