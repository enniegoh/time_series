# FORECASTING USING SMOOTHING, DECOMPOSITION, AND TIME TREND MODELS


# raw data
covid <- read.csv("covid.csv", header = T, strip.white = T)
head(covid)
sum(is.na(covid))
str(covid)

# From character to date
library(lubridate)
covid$date <- dmy(covid$date)
str(covid)
summary(covid)

# From data frame to time series
ts_covid <- ts(covid$cases_new, frequency = 365, start = c(2020, 1, 25), end = c(2023, 1, 31))
par(cex.lab = 2, cex.axis = 1.8)
plot(ts_covid, main = "Daily COVID-19 Cases in Malaysia From January 2020 to January 2023", xlab = "Date", 
     ylab = "Total New Cases", lwd = 2, 
     cex.main = 2)
rect(2021.43, 0, 2021.74, max(covid[covid$date <= "2021-12-01", ]$cases_new), col = adjustcolor("#E76F51", 0.2), lty = 0)
rect(2022.0, 0, 2022.27, max(ts_covid), col = adjustcolor("#da9d9c", 0.4), lty = 0)
abline(h = mean(ts_covid), lty = 2, col = "#da627d", lwd = 1)
text(x = 2020.5, y = 6000, label = paste("Average new \ncases: ", round(mean(ts_covid))))
arrows(x0 = 2021.2, x1 = 2021.4, y0 = 23000, y1 = max(covid[covid$date <= "2021-12-01", ]$cases_new), length = 0.15)
text(x = 2021.0, y = 22000, font = 4, col = "red", label = "26/08/2021")
arrows(x0 = 2022.5, x1 = 2022.3, y0 = 32000, y1 = max(ts_covid), length = 0.15)
text(x = 2022.7, y = 31000, font = 4, col = "red", label = "05/03/2022")
par(cex.lab = 1, cex.axis = 1)
plot(decompose(ts_covid))
library(forecast)
tsdisplay(ts_covid, main = "ACF and PACF", cex.main = 2)
library(tseries)
adf.test(ts_covid)   # unit root test

# Differencing
ndiffs(ts_covid)   # level of differencing
ts_diff_cases <- diff(ts_covid)
par(cex.lab = 1.5, cex.axis = 1.5)
plot(ts_diff_cases, main = "First Difference of Daily COVID-19 Cases in Malaysia From January 2020 to January 2023", 
     xlab = "Date", ylab = "First Difference", cex.main = 2)
adf.test(ts_diff_cases)
length(which(ts_diff_cases < 0))
lg_diff <- log10(ts_diff_cases + 1 - min(ts_diff_cases))
adf.test(lg_diff)

# Simple Exponential Smoothing
model_ses <- ses(lg_diff, alpha = 0.7, initial = "simple")
summary(model_ses)
par(cex.lab = 1.5, cex.axis = 1.5)
plot(forecast(model_ses), main = "Simple Exponential Smoothing Using First Differencing", xlab = "Date", ylab = "Total New Cases", 
     cex.main = 2)
lines(fitted(model_ses), col = "#c66363", lwd = 2)


# Linear Trend Model
model_regression1 <- tslm(ts_covid ~ trend + season)
summary(model_regression1)
summary(forecast(model_regression1))
par(cex.lab = 1.5, cex.axis = 1.5)
plot(forecast(model_regression1), main = "Linear Trend Model", xlab = "Date", ylab = "Total New Cases", cex.main = 2)
lines(fitted(model_regression1), col = "#c66363", lwd = 4)


model_regression2 <- tslm(ts_covid ~ trend)
summary(model_regression2)
summary(forecast(model_regression2))
par(cex.lab = 1.5, cex.axis = 1.5)
plot(forecast(model_regression2), main = "Linear Trend Model", xlab = "Date", ylab = "Total New Cases", cex.main = 2)
lines(fitted(model_regression2), col = "#c66363", lwd = 4)


# Additive Holt-Winters Exponential Smoothing
ts_hw <- HoltWinters(ts_covid, gamma = F, seasonal = "additive")
model_hw <- forecast(ts_hw, h = length(ts_covid))
summary(model_hw, h = 15)
par(cex.lab = 1.5, cex.axis = 1.5)
plot(forecast(model_hw, h = 15), main = "Additive Holt-Winters Exponential Smoothing", xlab = "Date", ylab = "Total New Cases", 
     cex.main = 2)
lines(fitted(model_hw), col = "#c66363", lwd = 2)


# Seasonal Trend Decomposition Using LOESS
ts_stl <- stl(ts_covid, t.window = 15, s.window = "periodic", robust = T)
model_stl <- forecast(ts_stl, method = "rwdrift", h = length(ts_covid))
summary(model_stl)
par(cex.lab = 1.5, cex.axis = 1.5)
plot(forecast(model_stl, h = 15), main = "Seasonal Trend Decomposition Using LOESS", xlab = "Date", ylab = "Total New Cases", 
     cex.main = 2)
lines(fitted(model_stl), col = "#c66363", lwd = 2)


# Moving Average - Method 3
library(smooth)

## 3 days moving average
model_sma_3 <- sma(lg_diff, order = 3, h = length(lg_diff))
model_sma_3
par(cex.lab = 1.5, cex.axis = 1.5)
plot(forecast(model_sma_3, h = 30), main = "Three Days Moving Average Using First Differencing", xlab = "Length of Data", 
     ylab = "First Differencing", cex.main = 2, lwd = 4)

## 7 days moving average
model_sma_7 <- sma(lg_diff, order = 7, h = length(lg_diff))
model_sma_7
par(cex.lab = 1.5, cex.axis = 1.5)
plot(forecast(model_sma_7, h = 30), main = "Seven Days Moving Average Using First Differencing", xlab = "Length of Data", 
     ylab = "First Differencing", cex.main = 2, lwd = 4)

## 15 days moving average
model_sma_15 <- sma(lg_diff, order = 15, h = length(lg_diff))
model_sma_15
par(cex.lab = 1.5, cex.axis = 1.5)
plot(forecast(model_sma_15, h = 30), main = "15 Days Moving Average Using First Differencing", xlab = "Length of Data", 
     ylab = "First Differencing", cex.main = 2, lwd = 4)

## 30 days moving average
model_sma_30 <- sma(lg_diff, order = 30, h = length(lg_diff))
model_sma_30
par(cex.lab = 1.5, cex.axis = 1.5)
plot(forecast(model_sma_30, h = 30), main = "30 Days Moving Average Using First Differencing", xlab = "Length of Data", 
     ylab = "First Differencing", cex.main = 2, lwd = 4)


## Plot
par(cex.lab = 1.5, cex.axis = 1.5)
plot(1:length(lg_diff), lg_diff, type = "l", main = "Moving Average Using First Differencing", xlab = "Length of Data", 
     ylab = "First Differencing", cex.main = 2, lwd = 4)
lines(1:length(lg_diff), model_sma_3$fitted, type = "l", col = "#f5d6ba", lwd = 4)
lines(1:length(lg_diff), model_sma_7$fitted, type = "l", col = "#da627d", lwd = 4)
lines(1:length(lg_diff), model_sma_15$fitted, type = "l", col = "#acc3a6", lwd = 4)
lines(1:length(lg_diff), model_sma_30$fitted, type = "l", col = "#f49d6e", lwd = 4)
legend(x = "bottomleft", 
       legend = c("Time Series", "3 Days Moving Average", "7 Days Moving Average", "15 Days Moving Average", "30 Days Moving Average"), 
       lty = 1, col = c("black", "#f5d6ba", "#da627d", "#acc3a6", "#f49d6e"), bty = "n", lwd = 4)


# Model Evaluation - Best Model?
## RMSE
rmse_ses <- forecast::accuracy(model_ses)[, "RMSE"]
rmse_hw <- forecast::accuracy(model_hw)[, "RMSE"]
rmse_stl <- forecast::accuracy(model_stl)[, "RMSE"]
rmse_regression1 <- forecast::accuracy(model_regression1)[, "RMSE"]
rmse_regression2 <- forecast::accuracy(model_regression2)[, "RMSE"]

## MAE
mae_ses <- forecast::accuracy(model_ses)[, "MAE"]
mae_hw <- forecast::accuracy(model_hw)[, "MAE"]
mae_stl <- forecast::accuracy(model_stl)[, "MAE"]
mae_regression1 <- forecast::accuracy(model_regression1)[, "MAE"]
mae_regression2 <- forecast::accuracy(model_regression2)[, "MAE"]

## RMSE
library(Metrics)
rmse_ma3 <- rmse(lg_diff, unlist(predict(model_sma_3)["mean"]))
rmse_ma7 <- rmse(lg_diff, unlist(predict(model_sma_7)["mean"]))
rmse_ma15 <- rmse(lg_diff, unlist(predict(model_sma_15)["mean"]))
rmse_ma30 <- rmse(lg_diff, unlist(predict(model_sma_30)["mean"]))

## MAE
mae_ma3 <- mae(lg_diff, unlist(predict(model_sma_3)["mean"]))
mae_ma7 <- mae(lg_diff, unlist(predict(model_sma_7)["mean"]))
mae_ma15 <- mae(lg_diff, unlist(predict(model_sma_15)["mean"]))
mae_ma30 <- mae(lg_diff, unlist(predict(model_sma_30)["mean"]))

## Compile the RMSE and MAE into data frame
model <- c("Simple Exponential Smoothing", "Holt-Winter Model", "Seasonal-Trend Decomposition", "Linear Seasonal Trend", 
           "Linear Trend", "3 Days Moving Average", "7 Days Moving Average", "15 Days Moving Average", "30 Days Moving Average")
rmse <- c(rmse_ses, rmse_hw, rmse_stl, rmse_regression1, rmse_regression2, rmse_ma3, rmse_ma7, rmse_ma15, rmse_ma30)
mae <- c(mae_ses, mae_hw, mae_stl, mae_regression1, mae_regression2, mae_ma3, mae_ma7, mae_ma15, mae_ma30)
rmse_rank <- rank(rmse)
mae_rank <- rank(mae)
aic <- c(rep(NA, 5), AIC(model_sma_3), AIC(model_sma_7), AIC(model_sma_15), AIC(model_sma_30))
aic_rank <- ifelse(!is.na(aic), rank(aic), NA)
performance <- data.frame(model, rmse, rmse_rank, mae, mae_rank, aic, aic_rank)
performance


