## Supply Chain Game 2
library(reshape2)
library(ggplot2)
library(dplyr)
library(lubridate)

# Load historical demand data
# Full, 730-day historical data is available for Calopeia
# Historical data for other regions only available >= day 640
demand <- read.csv('https://raw.githubusercontent.com/shenlim/MSCI3920/master/demand.csv')

demand_oth <- data.frame(demand[640:nrow(demand), names(demand) != 'Calopeia'])

# Crude forecast for Calopeia's demand
# Higher weights indicate a higher emphasis on Year 2 data
# For simple average, set weight = 0.5
forecast_avg_cal <- function(weight) {
  while (weight < 0 || weight > 1) {
    stop('Weights must be between 0 and 1 inclusive.')
  }
  
  demand_cal <<- data.frame(demand[, c('day', 'Calopeia')])
  
  for (i in seq(nrow(demand_cal) / 2)) {
    demand_cal[nrow(demand_cal) + 1, ] <<-
      list(i + 730, demand_cal[i, 2] * (1 - weight) + demand_cal[i + 365, 2] * weight)
  }
}

# Set weight to 0.7 to generate forecasts; adjust as necessary
forecast_avg_cal(0.7)

# Plot daily Calopeia demand
plot_cal <- ggplot(data = melt(demand_cal, id.vars = 'day')) +
  geom_line(aes(x = day, y = value)) +
  facet_wrap(~variable) +
  ylab('demand')

plot_cal

# Plot daily demand for other regions
plot_oth <- ggplot(data = melt(demand_oth, id.vars = 'day')) +
  geom_line(aes(x = day, y = value)) +
  facet_wrap(~variable) +
  ylab('demand')

plot_oth

# Forecast Calopeia demand
# Populate dummy dates (non leap year) to simulate daily data 
forecast_cal <- data.frame('day' = demand_cal$day[1:365],
                           'date' = seq(from = as.Date('2018-01-01'), to = as.Date('2018-12-31'), by = 'day'),
                           'forecast' = demand_cal$Calopeia[731:(730 + 365)])

## We have a crude forecast of Calopeia's demand for the next 730 days
## However, we need to plan capacity around the highly seasonal demand
## Partition the daily forecasted data into weeks, and compute the daily average of each week
# Initialize a data frame of average daily demand for each week
forecast_cal_wk <- data.frame('week' = seq(53),
                         'forecast' = NA)

# Populate average daily forecasts for each week
forecast_cal_wk$forecast <- summarize(group_by(forecast_cal, week = floor_date(date, 'week')), average = mean(forecast))$average

# Plot
plot_weekly_cal <- ggplot(data = forecast_cal_wk) +
  geom_line(aes(x = week, y = forecast))

# This daily average forecast by week is more manageable than 365 separate forecasts
plot_weekly_cal
