# Load required packages
library(readxl)  # For reading Excel files
library(circular)  # For circular statistics and plots
library(lubridate)  # For date-time manipulation

# Read the Excel file
data <- read_excel()
dataq <- read_excel("C:/Users/HP/OneDrive/Desktop/Working Papers/Anyanwu/Waves.xlsx", 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))
# Ensure time is in POSIXct format
if (!inherits(dataq$time, "POSIXct")) {
  dataq$time <- as.POSIXct(dataq$time, format = "%Y-%m-%d %H:%M:%S")
}

# Extract hour from time for subsetting
dataq$hour <- hour(dataq$time)

# Define AM and PM hours
am_hours <- c(1, 4, 7, 10)  # 1:00 AM, 4:00 AM, 7:00 AM, 10:00 AM
pm_hours <- c(13, 16, 19, 22)  # 1:00 PM, 4:00 PM, 7:00 PM, 10:00 PM

# Subset data for AM and PM
am_data <- dataq[dataq$hour %in% am_hours, ]
pm_data <- dataq[dataq$hour %in% pm_hours, ]

# Task 1: Circular plot for AM wind direction
wind_dir_am <- am_data$`wind dir`
wind_dir_am_circ <- circular(wind_dir_am, units = "degrees", template = "geographics")
rose.diag(wind_dir_am_circ, bins = 36, main = "Wind Direction AM", prop = 1.5)

# Task 2: Circular plot for PM wind direction
wind_dir_pm <- pm_data$`wind dir`
wind_dir_pm_circ <- circular(wind_dir_pm, units = "degrees", template = "geographics")
rose.diag(wind_dir_pm_circ, bins = 36, main = "Wind Direction PM", prop = 1.5)

# Task 3: Joint circular plot of AM and PM wind directions
plot(wind_dir_am_circ, main = "Wind Direction AM and PM", col = "blue")
points(wind_dir_pm_circ, col = "red")
legend("topright", legend = c("AM", "PM"), col = c("blue", "red"), pch = c(16, 16), bty = "n")

# Task 4: Angular-linear regression (tide height on wind direction) for AM
tide_hight_am <- am_data$`Tide hight`
wind_dir_am_rad <- wind_dir_am * pi / 180
sin_wind <- sin(wind_dir_am_rad)
cos_wind <- cos(wind_dir_am_rad)
model_al <- lm(tide_hight_am ~ sin_wind + cos_wind)
summary(model_al)
# Plot
plot(wind_dir_am, tide_hight_am, xlab = "Wind Direction (degrees)", ylab = "Tide Height", main = "Tide Height vs Wind Direction (AM)")
wind_seq <- seq(0, 360, by = 10)
wind_seq_rad <- wind_seq * pi / 180
newdata <- data.frame(sin_wind = sin(wind_seq_rad), cos_wind = cos(wind_seq_rad))
pred_al <- predict(model_al, newdata = newdata)
lines(wind_seq, pred_al, col = "red")

# Task 5: Histogram for AM wave period (linear variable)
hist(am_data$`wav prd`, main = "Wave Period AM", xlab = "Wave Period (seconds)", breaks = 20)

# Task 6: Histogram for PM wave period (linear variable)
hist(pm_data$`wav prd`, main = "Wave Period PM", xlab = "Wave Period (seconds)", breaks = 20)

# Task 7: Joint plot of wind direction and wave period for AM and PM
data$period <- ifelse(data$hour %in% am_hours, "AM", "PM")
all_wave <- data.frame(
  direction = data$`wind dir`,
  period = data$`wav prd`,
  period_label = data$period
)
plot(all_wave$direction, all_wave$period, col = ifelse(all_wave$period_label == "AM", "blue", "red"), 
     xlab = "Wind Direction (degrees)", ylab = "Wave Period (seconds)", main = "Wind Direction vs Wave Period")
legend("topright", legend = c("AM", "PM"), col = c("blue", "red"), pch = 1, bty = "n")

# Task 8: Circular-circular regression (wave direction on wind direction) for all data
wind_dir_all <- data$`wind dir`
wav_dir_all <- data$`wav dir`
wind_dir_all_circ <- circular(wind_dir_all, units = "degrees", template = "geographics")
wav_dir_all_circ <- circular(wav_dir_all, units = "degrees", template = "geographics")
model_cc <- lm.circular(y = wav_dir_all_circ, x = wind_dir_all_circ, type = "c-c")
summary(model_cc)
# Plot (approximate)
plot(wind_dir_all, wav_dir_all, xlab = "Wind Direction (degrees)", ylab = "Wave Direction (degrees)", main = "Wave Direction vs Wind Direction")
# Note: Adding a regression line for circular-circular data is complex and omitted here.