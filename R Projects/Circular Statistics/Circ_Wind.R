# Load required libraries
library(tidyverse)
library(readxl)
library(circular)
library(circlize)

# ---------------------------
# Data Preparation
# ---------------------------

# Read raw data from Excel
raw_data <- read_excel("C:/Users/HP/OneDrive/Desktop/Working Papers/Anyanwu/wave period wind direction in degree.xlsx", 
                                                   sheet = "Sheet2")
View(wave_period_wind_direction_in_degree)
# Identify rows containing dates
date_rows <- which(str_detect(raw_data$X__1, "^2025"))

# Process each daily block
all_days <- list()
for(i in seq_along(date_rows)) {
  start_row <- date_rows[i]
  
  # Extract date
  current_date <- as.Date(raw_data[[start_row, 1]], 
                          format = "%Y-%m-%d %H:%M:%S")
  
  # Extract variable rows
  var_rows <- raw_data[(start_row+1):(start_row+7), ]
  
  # Create dataframe
  day_df <- as.data.frame(t(var_rows[, 2:9]))
  colnames(day_df) <- var_rows[[1]]
  
  # Add metadata
  day_df <- day_df %>%
    mutate(
      date = current_date,
      time = as.numeric(str_extract(time, "^\\d{2}")),
      across(c(2:7), as.numeric)
    )
  
  all_days[[i]] <- day_df
}

# Combine data and convert to circular format
combined_data <- bind_rows(all_days) %>%
  mutate(
    wind_dir_circ = circular(`wind dir`, units = "degrees", 
                             template = "geographics"),
    wav_dir_circ = circular(`wav dir`, units = "degrees",
                            template = "geographics"),
    wav_prd_circ = circular(`wav prd`, units = "degrees")
  )

# Separate AM/PM data
am_data <- combined_data %>% filter(time %in% c(1,4,7,10))
pm_data <- combined_data %>% filter(time %in% c(13,16,19,22))

# ---------------------------
# Task 1: AM Wind Direction
# ---------------------------
plot(am_data$wind_dir_circ, stack = TRUE, shrink = 1.5,
     main = "Circular Plot of AM Wind Directions")

# ---------------------------
# Task 2: PM Wind Direction
# ---------------------------
plot(pm_data$wind_dir_circ, stack = TRUE, shrink = 1.5,
     main = "Circular Plot of PM Wind Directions")

# ---------------------------
# Task 3: Joint AM/PM Wind Directions
# ---------------------------
plot(am_data$wind_dir_circ, col = rgb(1,0,0,0.5), 
     main = "Joint AM (Red) and PM (Blue) Wind Directions")
points(pm_data$wind_dir_circ, col = rgb(0,0,1,0.5))

# ---------------------------
# Task 4: Angular-Linear Regression
# ---------------------------
circ_lm <- lm.circular(y = am_data$wind_dir_circ,
                       x = am_data$`win spd`,
                       type = "c-l")
summary(circ_lm)

# ---------------------------
# Tasks 5-6: Wave Period Plots
# ---------------------------
par(mfrow = c(1,2))
plot(am_data$wav_prd_circ, main = "AM Wave Period")
plot(pm_data$wav_prd_circ, main = "PM Wave Period")
par(mfrow = c(1,1))

# ---------------------------
# Task 7: Joint Circular Plot
# ---------------------------
circos.par("points.overflow.warning" = FALSE)
circos.initialize(factors = "a", xlim = c(0,360))
circos.points(am_data$wind_dir_circ, y = am_data$wav_prd_circ,
              col = "red", pch = 16)
circos.points(pm_data$wind_dir_circ, y = pm_data$wav_prd_circ,
              col = "blue", pch = 16)
circos.clear()

# ---------------------------
# Task 8: Circular-Circular Regression
# ---------------------------
cc_reg <- lm.circular(y = am_data$wind_dir_circ,
                      x = pm_data$wind_dir_circ,
                      type = "c-c")
summary(cc_reg)