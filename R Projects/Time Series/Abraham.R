#calling the required packages

library(tidyverse)
library(forecast)
library(tseries)
library(urca)
library(TSstudio)
library(lubridate)
library(TSA)
library(tsDyn)

#load the data
library(readxl)
Cass <- read_excel("C:/Users/DELL/Desktop/Working Papers/Akpan/Cassava.xlsx")

tcass<- ts(Cass$x,start = c(1961),end = c(2022), frequency = 1 )
ltcass <- log(tcass)
mod.setar <- setar(ltcass, m=2, thDelay=1, nthresh=1, mL= 2, mH= 1)
mod.setar
summary(mod.setar)
mod.setar$model
mod.setar$str
mod.setar$coefficients
mod.setar$fitted.values
mod.setar$residuals
mod.setar$k
mod.setar$model.specific
mod.setar$include

getTh(mod.setar)
## forecast values
predict(mod.setar, n.ahead = 10)

# Plot forecasts
forecast_plot <- autoplot(forecast(arima_model, h = 10)) +
  labs(title = "ARIMA(3,2,4) Forecast for Cassava Prices till 2032",
       x = "Year",
       y = "Price") +
  theme_minimal()

forecast_plot

accuracy(f_arima)



cass.setar <- setar(tcass, 2, d=2, steps=d, series, mL, mM, mH, thDelay=2, th=2, trace=FALSE, 
      nested=FALSE, include = c( "const", "trend","none", "both"), 
      common=c("none", "include","lags", "both"), model=c("TAR", "MTAR"), ML=seq_len(mL), 
      MM=seq_len(mM), MH=seq_len(mH),nthresh=1,trim=0.15, type=c("level", "diff", "ADF"),
      restriction=c("none","OuterSymAll","OuterSymTh") )

model <- setar(tcass, 2, d=2, steps=d, series, 2, 0, 1, thDelay=2, th=2)

summary(model)

forecast_values <- predict(model, n.ahead = number_of_forecast_steps)

mod.setar <- setar(log10(tcass), m=2, thDelay=1, th=3.02)
mod.setar
summary(mod.setar)

######################################################################################
##ARIMA MODELLING
##############################################################################
# Load necessary libraries
library(readxl)
library(ggplot2)
library(forecast)
library(tseries)
library(zoo)

# Read the Excel file
Cassava <- read_excel("Cassava.xlsx")

# Print the structure of the data
str(Cass)

# Convert the 'year' column to numeric if it's not already
Cass$year <- as.numeric(Cass$year)

# Create a time series object
ts_Cass <- ts(Cass$x, start = min(Cass$year), frequency = 1)

# Print the first few values of the time series
print(head(ts_Cass))
str(ts_Cass)

# Plot the time series
plot(ts_Cass, main = "Cassava Data Time Series", ylab = "Value", xlab = "Year")

# Plot the time series
ggplot(ts_Cass, aes(x = ts_Cass[[1]], y = ts_Cass[[2]])) +
  geom_line() +
  labs(title = "Cassava Price Time Series",
       x = "Date",
       y = "Price") +
  theme_minimal()

# Save the plot
ggsave("cassava_time_series.png", width = 10, height = 6)

# Print summary statistics
summary(ts_Cass)

#####################################
# Perform Augmented Dickey-Fuller test to check for stationarity
adf_test <- adf.test(ts_Cass, alternative = "stationary")

# Plot ACF and PACF
acf(ts_Cass, main = "ACF of Cassava Prices")
pacf(ts_Cass, main = "PACF of Cassava Prices")

# Print the result of the ADF test
print(adf_test)

#############
# Difference the data to achieve stationarity
# First difference
diff_ts_Cass <- diff(ts_Cass)

# Perform Augmented Dickey-Fuller test on differenced data
adf_test_diff <- adf.test(diff_ts_Cass, alternative = "stationary")

# Plot ACF and PACF of differenced data
acf(diff_ts_Cass, main = "ACF of Differenced Cassava Prices")
pacf(diff_ts_Cass, main = "PACF of Differenced Cassava Prices")

# Print the result of the ADF test on differenced data
print(adf_test_diff)

### SECOND DIFFERENCING
# Second difference
ddiff_ts_Cass <- diff(diff_ts_Cass)

# Perform Augmented Dickey-Fuller test on differenced data
adf_test_ddiff <- adf.test(ddiff_ts_Cass, alternative = "stationary")

# Plot ACF and PACF of differenced data
acf(ddiff_ts_Cass, main = "ACF of 2nd Differenced Cassava Prices")
pacf(ddiff_ts_Cass, main = "PACF of 2nd Differenced Cassava Prices")

# Print the result of the ADF test on differenced data
print(adf_test_ddiff)

######## Model Selection
# Fit ARIMA(2,1,2) model
arima_model <- arima(ts_Cass, order = c(2,1,2))
# Print AIC
AIC(arima_model)
# Fit ARIMA(1,1,0) model
arima_model <- arima(ts_Cass, order = c(1,1,0))
# Print AIC
AIC(arima_model)
# Fit ARIMA(0,1,1) model
arima_model <- arima(ts_Cass, order = c(0,1,1))
# Print AIC
AIC(arima_model)
# Fit ARIMA(1,1,1) model
arima_model <- arima(ts_Cass, order = c(1,1,1))
# Print AIC
AIC(arima_model)
# Fit ARIMA(3,1,1) model
arima_model <- arima(ts_Cass, order = c(3,1,1))
# Print AIC
AIC(arima_model)
# Fit ARIMA(3,1,4) model
arima_model <- arima(ts_Cass, order = c(3,1,4))
# Print AIC
AIC(arima_model)

# Fit ARIMA(3,2,4) model
arima_model <- arima(ts_Cass, order = c(3,2,4))
# Print AIC
AIC(arima_model)
# Print model summary
print(summary(arima_model))
# Perform Ljung-Box test
lb_test <- Box.test(arima_model$residuals, lag = 10, type = "Ljung-Box")
print(lb_test)
# Plot model diagnostics
tsdiag(arima_model)
## forecast values
f_arima <- forecast(arima_model, h = 10)

# Plot forecasts
forecast_plot <- autoplot(forecast(arima_model, h = 10)) +
  labs(title = "ARIMA(3,2,4) Forecast for Cassava Prices till 2032",
       x = "Year",
       y = "Price") +
  theme_minimal()

forecast_plot

accuracy(f_arima)


