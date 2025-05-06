# Load libraries
library(ggplot2)
library(forecast)
library(tsibble)
library(fable)
library(fabletools)
library(tibble)
library(dplyr)
library(Metrics)
library(urca)
library(zoo)

data("AirPassengers")

# Convert to data frame with proper Date
df <- data.frame(
  date = as.Date(as.yearmon(time(AirPassengers))),
  value = as.numeric(AirPassengers)
)

# Visualize with ggplot2
ggplot(df, aes(x = date, y = value)) +
  geom_line(color = "steelblue") +
  labs(title = "Monthly Airline Passengers (1949–1960)",
       x = "Year", y = "Number of Passengers") +
  theme_minimal()

# Convert to tsibble
df_tsibble <- df %>%
  mutate(month = yearmonth(date)) %>%
  as_tsibble(index = month)

# Convert to time series
ts_data <- ts(df$value, frequency = 12, start = c(1949, 1))

# Perform ADF test
adf_test <- ur.df(ts_data, type = "trend", selectlags = "AIC")
summary(adf_test)


# Convert AirPassengers to time series (if not already done)
ts_data <- ts(df$value, frequency = 12, start = c(1949, 1))

# Apply log transformation
log_ts <- log(ts_data)  # This is the line you missed before

arima_model <- auto.arima(log_ts)
summary(arima_model)

# Forecast 24 months ahead
arima_forecast <- forecast(arima_model, h = 24)
ets_forecast <- forecast(ets_model, h = 24)

# Plot forecasts
autoplot(arima_forecast) + ggtitle("ARIMA Forecast (Log Scale)")
autoplot(ets_forecast) + ggtitle("ETS Forecast (Log Scale)")


# Fit ETS model on log-transformed data
ets_model <- ets(log_ts)
summary(ets_model)

# Forecast next 24 months using ARIMA and ETS
arima_forecast <- forecast(arima_model, h = 24)
ets_forecast <- forecast(ets_model, h = 24)

# Plot the forecasts
autoplot(arima_forecast) + ggtitle("ARIMA Forecast (Log Scale)")
autoplot(ets_forecast) + ggtitle("ETS Forecast (Log Scale)")

# Inverse log transformation for ARIMA and ETS forecasts
arima_forecast_exp <- exp(arima_forecast$mean)
ets_forecast_exp <- exp(ets_forecast$mean)

# Plot the forecasts on the original scale
autoplot(arima_forecast_exp) + ggtitle("ARIMA Forecast (Original Scale)")
autoplot(ets_forecast_exp) + ggtitle("ETS Forecast (Original Scale)")

# Create train and test sets
train <- window(log_ts, end = c(1958, 12))
test <- window(log_ts, start = c(1959, 1))

# Refit on training data
arima_fit <- auto.arima(train)
ets_fit <- ets(train)

# Forecast on test set
arima_pred <- forecast(arima_fit, h = length(test))
ets_pred <- forecast(ets_fit, h = length(test))

# Inverse log transform to original scale
arima_rmse <- rmse(exp(test), exp(arima_pred$mean))
ets_rmse <- rmse(exp(test), exp(ets_pred$mean))

arima_mae <- mae(exp(test), exp(arima_pred$mean))
ets_mae <- mae(exp(test), exp(ets_pred$mean))

# Print results
cat("ARIMA - RMSE:", arima_rmse, "MAE:", arima_mae, "\n")
cat("ETS   - RMSE:", ets_rmse, "MAE:", ets_mae, "\n")

