# Load required packages
library(readxl)
library(circular)

# Read the Excel file
data <- read_excel("C:/Users/HP/OneDrive/Desktop/Working Papers/Anyanwu/wave period wind direction in degree.xlsx", 
                   sheet = "Sheet3")
View(data)
# Define AM and PM times based on fractional days
am_times <- c(0.041666666666666664, 0.16666666666666666, 0.2916666666666667, 0.4166666666666667)  # 1:00 AM, 4:00 AM, 7:00 AM, 10:00 AM
pm_times <- c(0.5416666666666666, 0.6666666666666666, 0.7916666666666666, 0.9166666666666666)    # 1:00 PM, 4:00 PM, 7:00 PM, 10:00 PM
# Add period column to retain AM/PM information
data$period <- ifelse(data$time < 0.5, "AM", "PM")

# Task 1: Circular plot for AM wind direction

# Define AM times as fractional days
# 1:00 AM = 1/24 ≈ 0.0416667, 4:00 AM = 4/24 ≈ 0.1666667, 
# 7:00 AM = 7/24 ≈ 0.2916667, 10:00 AM = 10/24 ≈ 0.4166667
am_times <- c(0.041666666666666664, 0.16666666666666666, 
              0.2916666666666667, 0.4166666666666667)

# Convert the time column to numeric (fractional days)
# If time is a POSIXt object, convert it to numeric days since the epoch, then extract the fractional part
data$time_numeric <- as.numeric(data$time) %% 1  # Extract fractional day part (0 to 1)

# Use approximate matching to select AM times, accounting for floating-point precision
epsilon <- 1e-6  # Small tolerance for matching
am_indices <- sapply(data$time_numeric, function(t) any(abs(t - am_times) < epsilon))
wind_dir_am <- data$`wind dir`[am_indices]

# Check if any data is selected to avoid plotting an empty diagram
if (length(wind_dir_am) == 0) {
  stop("No data selected for AM times. Check time values and subsetting.")
}

# Convert wind direction data to a circular object
wind_dir_am_circ <- circular(wind_dir_am, units = "degrees", template = "geographics")

# Create a wind rose diagram to show the distribution of wind directions
rose.diag(wind_dir_am_circ, 
          bins = 36,           # Number of bins for direction (10° intervals)
          main = "Wind Direction AM",  # Title of the plot
          prop = 1.5)          # Scaling factor for the radius of the plot



#Task 111
# Define AM times based on fractional days
am_times <- c(0.041666666666666664, 0.16666666666666666, 0.2916666666666667, 0.4166666666666667)  # 1:00 AM, 4:00 AM, 7:00 AM, 10:00 AM

# Subset the data for AM times
am_data <- data[data$time %in% am_times, ]

# Extract wind direction for AM times (assuming the column is named "wind dir")
wind_dir_am <- am_data$`wind dir`

# Convert wind direction to a circular object
wind_dir_am_circ <- circular(wind_dir_am, units = "degrees", template = "geographics")

# Create a circular plot for AM wind directions
plot(wind_dir_am_circ, stack = TRUE, bins = 36, shrink = 1.5, main = "Wind Direction aM")

# Task 2: Circular plot for PM wind direction
wind_dir_pm <- data$wind_dir[data$time %in% pm_times]
wind_dir_pm_circ <- circular(wind_dir_pm, units = "degrees", template = "geographics")
plot(wind_dir_pm_circ, stack = TRUE, bins = 36, shrink = 1.5, main = "Wind Direction PM")

# Task 3: Joint circular plot of AM and PM wind directions
plot(wind_dir_am_circ, points.col = "blue", points.pch = 16, main = "Wind Direction AM and PM")
points(wind_dir_pm_circ, points.col = "red", points.pch = 17)
legend("topright", legend = c("AM", "PM"), col = c("blue", "red"), pch = c(16, 17))

# Task 4: Angular-linear regression (e.g., tide height on wind direction)
tide_hight_am <- data$tide_hight[data$time %in% am_times]
wind_dir_rad_am <- wind_dir_am * pi / 180  # Convert degrees to radians
sin_wind <- sin(wind_dir_rad_am)
cos_wind <- cos(wind_dir_rad_am)
model_al <- lm(tide_hight_am ~ sin_wind + cos_wind)
summary(model_al)
# Plot the relationship
plot(wind_dir_am, tide_hight_am, xlab = "Wind Direction (degrees)", ylab = "Tide Height", main = "Tide Height vs Wind Direction")
wind_seq <- seq(0, 360, by = 10)
wind_seq_rad <- wind_seq * pi / 180
pred_al <- predict(model_al, newdata = data.frame(sin_wind = sin(wind_seq_rad), cos_wind = cos(wind_seq_rad)))
lines(wind_seq, pred_al, col = "red")

# Task 5: Circular plot for AM wave period
wav_prd_am <- data$wav_prd[data$time %in% am_times]
wav_prd_am_circ <- circular(wav_prd_am, units = "degrees", template = "geographics")
plot(wav_prd_am_circ, stack = TRUE, bins = 36, shrink = 1.5, main = "Wave Period AM")

# Task 6: Circular plot for PM wave period
wav_prd_pm <- data$wav_prd[data$time %in% pm_times]
wav_prd_pm_circ <- circular(wav_prd_pm, units = "degrees", template = "geographics")
plot(wav_prd_pm_circ, stack = TRUE, bins = 36, shrink = 1.5, main = "Wave Period PM")

# Task 7: Joint circular plot of AM and PM for wind direction and wave period
plot(wind_dir_am_circ, points.col = "blue", points.pch = 16, main = "Wind Direction and Wave Period AM and PM")
points(wind_dir_pm_circ, points.col = "red", points.pch = 17)
points(wav_prd_am_circ, points.col = "green", points.pch = 18)
points(wav_prd_pm_circ, points.col = "purple", points.pch = 19)
legend("topright", legend = c("Wind Dir AM", "Wind Dir PM", "Wave Prd AM", "Wave Prd PM"), 
       col = c("blue", "red", "green", "purple"), pch = c(16, 17, 18, 19))

# Task 8: Circular-circular regression (e.g., wave direction on wind direction)
wind_dir_all <- data$wind_dir
wav_dir_all <- data$wav_dir
wind_dir_all_circ <- circular(wind_dir_all, units = "degrees", template = "geographics")
wav_dir_all_circ <- circular(wav_dir_all, units = "degrees", template = "geographics")
model_cc <- lm.circular(y = wav_dir_all_circ, x = wind_dir_all_circ, type = "c-c")
# Plot the relationship
plot(wind_dir_all, wav_dir_all, xlab = "Wind Direction (degrees)", ylab = "Wave Direction (degrees)", 
     main = "Wave Direction vs Wind Direction")
wind_seq <- seq(0, 360, by = 10)
wind_seq_circ <- circular(wind_seq, units = "degrees", template = "geographics")
pred_cc <- predict(model_cc, newdata = wind_seq_circ)
lines(wind_seq, pred_cc, col = "red")