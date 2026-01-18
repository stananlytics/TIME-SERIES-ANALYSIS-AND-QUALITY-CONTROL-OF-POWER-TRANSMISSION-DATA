# Summarizing our data set for phase 2
str(md)
head(md)

# installing our packages
install.packages("tseries")
library("tseries")
install.packages("forecast")
library("forecast")
install.packages("psych")
library("psych")
# converting our data model to fit the time series analysis
ts_GT1 <- ts(md$`GT 1`, start = c(2025, 3, 1), frequency = 365)
ts_GT2 <- ts(md$`GT 2`, start = c(2025, 3, 1), frequency = 365)
ts_GT3 <- ts(md$`GT 3`, start = c(2025, 3, 1), frequency = 365)
ts_GT4 <- ts(md$`GT 4`, start = c(2025, 3, 1), frequency = 365)# March data
ts_GT1
ts_GT2
ts_GT3
ts_GT4

# The data plot

# Step 4: Plot all GT series in one line chart

plot(md$DAYS, md$`GT 1`, type = "l", col = "blue", lwd = 2,
     ylim = range(c(md$`GT 1`, md$`GT 2`, md$`GT 3`, md$`GT 4`), na.rm = TRUE),
     xlab = "Day of March", ylab = "Values", main = "Daily Time Series for GT 1 to GT 4")

# Add lines for GT 2 to GT 4
lines(md$DAYS, md$`GT 2`, col = "green", lwd = 2)
lines(md$DAYS, md$`GT 3`, col = "red", lwd = 2)
lines(md$DAYS, md$`GT 4`, col = "purple", lwd = 2)

# Add a legend
legend("topright", legend = c("GT 1", "GT 2", "GT 3", "GT 4"),
       col = c("blue", "green", "red", "purple"), lty = 1, lwd = 2)





# Define the columns to be tested
selected_columns <- c("GT1", "GT2", "GT3", "GT4")

# Step 1: Run Augmented Dickey-Fuller (ADF) Test for each GT column
for (GT in selected_columns) {
  cat("\nADF Test for", GT, "\n")
  
  data_vec <- md[[GT]]
  
  if (length(data_vec) > 0 && sum(!is.na(data_vec)) > 0) {
    ts_data <- ts(na.omit(data_vec), frequency = 365)  # Weekly data assumed
    print(adf.test(ts_data))
  } else {
    cat("Skipped: Column is empty or all values are NA.\n")
  }
}



# checking the stationary of our data

adf.test(md$`GT 1`)
adf.test(md$`GT 2`)
adf.test(md$`GT 3`)
adf.test(md$`GT 4`)


# since our data are not stationary, we will use the autoarima


# Step 1: Select only GT columns that have valid (non-NA) values
valid_selected_columns <- names(md)[sapply(md, function(x) sum(!is.na(x)) > 0)]

# Step 2: Fit ARIMA models only for valid columns
selected_columns <- lapply(valid_selected_columns, function(col) {
  series <- ts(na.omit(md[[col]]), frequency = 365)  # daily data
  auto.arima(series, ic = "aic", trace = TRUE)
})
names(selected_columns) <- valid_selected_columns

# Forecast only for March 2025
forecast_horizon <- 31   # March has 31 days

# Step 2: Forecast 31 days ahead for each model in selected_columns
gt_forecasts <- lapply(seq_along(selected_columns), function(i) {
  model <- selected_columns[[i]]
  gt_name <- names(selected_columns)[i]
  
  cat("\nForecast for", gt_name, " (March 2025):\n")
  fc <- forecast(model, h = forecast_horizon)
  print(summary(fc))
  
  plot(fc, main = paste("Forecast for", gt_name, "- March 2025"))
  return(fc)
})

# Combine GT1 to GT3 with selected_columns into one list
all_models <- c(selected_columns, list(GT1 = `GT 1`, `GT 2` = GT2, GT3 = `GT 3`))

forecast_horizon <- 31  # for March 2025

# Forecast 31 days ahead for each model
gt_forecasts <- lapply(seq_along(all_models), function(i) {
  model <- all_models[[i]]
  gt_name <- names(all_models)[i]
  
  cat("\nForecast for", gt_name, " (March 2025):\n")
  fc <- forecast(model, h = forecast_horizon)
  print(summary(fc))
  
  plot(fc, main = paste("Forecast for", gt_name, "- March 2025"))
  return(fc)
})

names(gt_forecasts) <- names(all_models)

# Assign names to the forecasts
names(gt_forecasts) <- names(selected_columns)

describe(md$`GT 1`)
describe(md$`GT 2`)
describe(md$`GT 3`)
describe(md$`GT 4`)





# Analysis for phase 1
str(md)
head(md)

# descriptive

describe(phase_1$`GT 1`)
describe(phase_1$`GT 2`)
describe(phase_1$`GT 3`)
describe(phase_1$`GT 4`)
describe(phase_1$`GT 5`)
describe(phase_1$`GT 6`)
describe(phase_1$`GT 7`)
describe(phase_1$`GT 8`)

# converting our data for the time series analysis
# converting our data model to fit the time series analysis
tp_GT1 <- ts(phase_1$`GT 1`, start = c(2025, 3, 1), frequency = 365)
tp_GT2 <- ts(phase_1$`GT 2`, start = c(2025, 3, 1), frequency = 365)
tp_GT3 <- ts(phase_1$`GT 3`, start = c(2025, 3, 1), frequency = 365)
tp_GT4 <- ts(phase_1$`GT 4`, start = c(2025, 3, 1), frequency = 365)
tp_GT5 <- ts(phase_1$`GT 5`, start =c(2025, 3,1 ), frequency = 365)
tp_GT6 <- ts(phase_1$`GT 6`, start =c(2025, 3, 1), frequency = 365)
tp_GT7 <- ts(phase_1$`GT 7`, start = c(2025, 3, 1), frequency =365)
tp_GT8 <- ts(phase_1$`GT 8`, start = c(2025, 3, 1), frequency = 365)# March data
tp_GT1
tp_GT2
tp_GT3
tp_GT4
tp_GT6
tp_GT7
tp_GT8

# Reset plotting layout to a single plot
par(mfrow = c(1, 1))  # Only 1 plot at a time
par(mar = c(5, 5, 4, 2))  # Increase margins: bottom, left, top, right

# Calculate y-axis range across all GTs
ylim <- range(c(phase_1$`GT 1`, phase_1$`GT 2`, phase_1$`GT 3`, phase_1$`GT 4`, 
                phase_1$`GT 5`, phase_1$`GT 6`, phase_1$`GT 7`, phase_1$`GT 8`), na.rm = TRUE)

# Plot GT 1
plot(phase_1$DAYS, phase_1$`GT 1`, type = "l", col = "lightgreen", lwd = 2,
     ylim = ylim, xlab = "Days of March", ylab = "GT Values",
     main = "Daily Time Series for GT 1 to GT 8", cex.main = 1.5, cex.lab = 1.2, cex.axis = 1)

# Add lines for GT 2 to GT 8
lines(phase_1$DAYS, phase_1$`GT 2`, col = "blue", lwd = 2)
lines(phase_1$DAYS, phase_1$`GT 3`, col = "red", lwd = 2)
lines(phase_1$DAYS, phase_1$`GT 4`, col = "purple", lwd = 2)
lines(phase_1$DAYS, phase_1$`GT 5`, col = "orange", lwd = 2)
lines(phase_1$DAYS, phase_1$`GT 6`, col = "lightgrey", lwd = 2)
lines(phase_1$DAYS, phase_1$`GT 7`, col = "yellow", lwd = 2)
lines(phase_1$DAYS, phase_1$`GT 8`, col = "skyblue", lwd = 2)

# Add a clear legend
legend("topright", legend = c("GT 1", "GT 2", "GT 3", "GT 4", "GT 5", "GT 6", "GT 7", "GT 8"),
       col = c("lightgreen", "blue", "red", "purple", "orange", "lightgrey", "yellow", "skyblue"),
       lty = 1, lwd = 2, cex = 0.9)


selected_columns <- phase_1[c("GT 1", "GT 2", "GT 3", "GT 4", "GT 5", "GT 6", 
                              "GT 7", "GT 8")]
selected_columns




# Load required packages
library(tseries)
library(forecast)

# Load required libraries
library(tseries)
library(forecast)

# Define the GT columns to be tested and forecast
selected_columns <- c("GT 1", "GT 2", "GT 3", "GT 4", "GT 5", "GT 6", "GT 7", "GT 8")

# Step 1: Run Augmented Dickey-Fuller (ADF) Test for each GT column in phase_1
for (GT in selected_columns) {
  cat("\nADF Test for", GT, "\n")
  
  data_vec <- phase_1[[GT]]
  
  if (length(data_vec) > 0 && sum(!is.na(data_vec)) > 0) {
    ts_data <- ts(na.omit(data_vec), frequency = 7)  # Weekly pattern assumed
    print(adf.test(ts_data))
  } else {
    cat("Skipped: Column is empty or all values are NA.\n")
  }
}
# checking the stationary of our data
acf(phase_1$`GT 1`)
acf(phase_1$`GT 2`)
acf(phase_1$`GT 3`)
acf(phase_1$`GT 4`)
adf.test(phase_1$`GT 5`)
acf(phase_1$`GT 5`)
acf(phase_1$`GT 6`)
acf(phase_1$`GT 7`)
acf(phase_1$`GT 8`)

adf.test(phase_1$`GT 1`)
adf.test(phase_1$`GT 2`)
adf.test(phase_1$`GT 3`)
adf.test(phase_1$`GT 4`)
adf.test(phase_1$`GT 6`)
adf.test(phase_1$`GT 7`)
adf.test(phase_1$`GT 8`)


library(forecast)



# using the arima model

gt_models <- lapply(phase_1, function(series) {
  auto.arima(series, ic = "aic", trace = TRUE)
})

# Check each model and plot only if residuals are safe
for (i in seq_along(gt_models)) {
  model <- gt_models[[i]]
  res <- tryCatch(residuals(model), error = function(e) NA)
  
  # Print info for debugging
  cat("\nGT", i, " - Residual summary:\n")
  print(summary(res))
  
  # Only plot if numeric, finite, and has variability
  if (is.numeric(res) && any(is.finite(res)) && diff(range(res, na.rm = TRUE)) > 0) {
    acf(res, main = paste("ACF of Residuals - GT", i))
  } else {
    message(paste("⚠️ Skipping GT", i, "- residuals invalid or constant"))
  }
}



# forecasting our model



# Clean column names
names(phase_1) <- trimws(names(phase_1))

# List of best models you determined earlier
model_specs <- list(
  `GT 1` = list(order = c(1, 0, 0), include.mean = TRUE),
  `GT 2`= list(order = c(1, 0, 0), include.mean = TRUE),
  `GT 3` = list(order = c(0, 1, 0)),
  `GT 4` = list(order = c(0, 0, 1), include.mean = TRUE),
  `GT 5` = list(order = c(1, 0, 0), include.mean = TRUE),
  `GT 6` = list(order = c(0, 1, 0)),
  `GT 7` = list(order = c(1, 0, 0), include.mean = TRUE),
  `GT 8`= list(order = c(0, 1, 0))
)
