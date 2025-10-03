  ## Modelling Daily COVID-19 cases of Nigeria using Prophet in r

  library(prophet)
  library(tidyverse)
  library(readxl)

  #Daily coronavirus cases
  CT <- read_excel("C:/Users/DELL/Desktop/Working Papers/Tina/COVID Tina.xlsx", 
                           sheet = "Total Cases")
  View(CT)
  head(CT)
  tail(CT)
  df <- ts(CT$Total_cases, start = c(2020,02,28),frequency = 365)
  head(df)
  
  h <- data.frame(ds = seq(as.Date('2020-02-28'), as.Date('2021-09-30'), by = 'd'),
                        y = CT$Total_cases)
    head(h)
  # Call the prophet function and fit the model
  
  model1 <- prophet(h)
  future1 <- make_future_dataframe(model1, periods = 30)
  tail(future1)
  
  # Forecast
  
  forecast1 <- predict(model1, future1)
  tail(forecast1[c('ds','yhat','yhat_lower','yhat_upper')])
  
  # Plot the estimates
  
  dyplot.prophet(model1, forecast1)