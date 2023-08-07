# ARIMA Model

# data
hfmd <- read.csv("2010-2021.csv", header = T, strip.white = T)
head(hfmd)
hfmd <- hfmd[, c(1, 2, 3, 19)]
names(hfmd) <- c("date", "year", "week", "cases")
names(hfmd)
str(hfmd)

library(tidyverse)
library(lubridate)
library(zoo)

hfmd <- hfmd %>% group_by(month = as.Date(as.yearmon(dmy(date)), 1)) %>% 
  summarise(total = sum(cases))
sum(is.na(hfmd))
summary(hfmd)
nrow(hfmd)


ts_hfmd <- ts(hfmd$total, frequency = 12, start = c(2010, 1), end = c(2021, 12))
length(ts_hfmd)
ts.plot(ts_hfmd, main = "Total HFMD Cases in Malaysia From 2010 to 2021", xlab = "Year", ylab = "Number of HFMD Cases")
plot(decompose(ts_hfmd))

library(tseries)
adf.test(ts_hfmd)

library(forecast)
ndiffs(ts_hfmd)

ts_diff_hfmd <- diff(ts_hfmd, differences = 1)
length(ts_diff_hfmd)
plot.ts(ts_diff_hfmd, main = "Total HFMD Cases in Malaysia From 2010 to 2021 at First Difference", xlab = "Year", 
        ylab = "First Difference")

adf.test(ts_diff_hfmd)

par(mfrow = c(1, 2))
acf(ts_diff_hfmd, main = "Autocorrelation Function")
acflg <- acf(ts_diff_hfmd, plot = F)
acflg$lag <- acflg$lag * 12
plot(acflg, main = "Autocorrelation Function(months)", xlab = "Lag(months)")

pacf(ts_diff_hfmd, main = "Partial Autocorrelation Function")
pacflg <- pacf(ts_diff_hfmd, plot = F)
pacflg$lag <- pacflg$lag * 12
plot(pacflg, main = "Partial Autocorrelation Function(months)", xlab = "Lag(months)")


model1 <- arima(ts_hfmd, order = c(0, 1, 2), seasonal = list(order = c(1, 0, 0)))
model1
library(lmtest)
coeftest(model1)

par(mfrow = c(1, 1))
{
  plot.ts(ts_hfmd, main = "Total HFMD Cases in Malaysia in 2010 - 2021", ylab = "Number of HFMD Cases")
  lines(fitted(model1), col = "red", lty = 2)
  legend(x = "topleft", legend = c("Observed Data", "Fitted Model"), col = c("black", "red"), lty = c(1, 2))
}


fitted_value <- forecast(model1, h = 5)
resid <- fitted_value$residuals

hist(resid, main = "Histogram of Residuals", xlab = "Residuals")

resid_acflg <- acf(resid, plot = F)
resid_acflg$lag <- resid_acflg$lag * 12
plot(resid_acflg, main = "Autocorrelation Function of Residuals(months)", xlab = "Lag(months)")

Box.test(resid, lag = 21, type = "Ljung-Box")

plot.ts(resid, main = "Residuals", xlab = "Year", ylab = "Residuals")

auto.arima(ts_hfmd)


hfmd_forecast <- forecast(model1, h = 24)
plot(hfmd_forecast, xlab = "Year", ylab = "Number of HFMD Cases")


ts_hfmd_data <- tbats(ts_hfmd)
hfmd_forecast <- forecast(ts_hfmd_data, h = 24)
plot(hfmd_forecast, xlab = "Year", ylab = "Number of HFMD Cases")



