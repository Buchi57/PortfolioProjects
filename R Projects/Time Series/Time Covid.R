#calling the required packages

library(tidyverse)
library(forecast)
library(tseries)
library(urca)
library(TSstudio)
library(lubridate)

#load the data
library(readxl)
COVID_Tina <- read_excel("C:/Users/DELL/Desktop/Working Papers/Tina/COVID Tina.xlsx")
View(COVID_Tina)
head(COVID_Tina)
nrow(COVID_Tina)

  # Change to daily data
  minday = as.Date("2020-02-28")
  maxday = as.Date("2021-09-30")
  dates <- seq(minday, maxday, "days")
  # create some holes
  dates <- dates[sample(1:length(dates), length(dates)/4)]
  df <- data.frame(date=sort(dates), val=seq(as.Date('2020-02-28'), as.Date('2021-09-30'), by = 'd'),
                   y = CT$Total_cases)  

## Declare our time series variable

Tocs<- ts(COVID_Tina$Total_cases,start = c(2020,2,28),end = c(2021,9,30), frequency = 365.25 )
Tods<- ts(COVID_Tina$Total_deaths, start = c(2020,02,28),frequency = 365.25)

## Plotting the time series object
autoplot(Tocs)+ ggtitle("Plot of the total cases of covid-19")+ labs(x = "Time",y = "Total cases")
autoplot(Tods)+ ggtitle("Plot of the total deaths of covid-19")+ labs(x = "Time",y = "Total deaths")

# Generate some descriptives
summary(Tocs)
summary(Tods)
 
 #Look at the Acf and Pacf

  ggAcf(Tocs, lag.max = 400) + ggtitle("Acf of the total cases")
  ggAcf(Tods,lag.max = 400) + ggtitle("Acf of the total deaths")

  ggPacf(Tocs,lag.max = 50) + ggtitle("Pacf of the total cases")
  ggPacf(Tods,lag.max = 50) + ggtitle("Pacf of the total deaths")
  
  # Difference the series
  dtocs <- diff(Tocs)
  dtods <- diff(Tods)
  
  ggAcf(dtocs, lag.max = 400) + ggtitle("Acf of the total cases")
  ggAcf(dtods,lag.max = 400) + ggtitle("Acf of the total deaths")
  
  ggPacf(dtocs,lag.max = 50) + ggtitle("Pacf of the total cases")
  ggPacf(dtods,lag.max = 50) + ggtitle("Pacf of the total deaths")
  
  # Decompose the series
  
  ts_decompose(Tocs, type ="additive", showline = TRUE)
  ts_decompose(Tods, type = "additive", showline = TRUE)