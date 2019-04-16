# Supply Chain Game 2
library(reshape2)
library(ggplot2)

# Load historical demand data
# Full, 730-day historical data is available for Calopeia
# Historical data for other regions only available >= day 640
demand <- read.csv('2019-04-15 - Demand history.csv', header = TRUE)

demand_cal <- data.frame(day = demand[1:365, 'day'],
                         year1 = demand[1:365, 'Calopeia'],
                         year2 = demand[366:730, 'Calopeia'])

demand_oth <- data.frame(demand[640:nrow(demand), names(demand) != 'Calopeia'])

# Crude forecast for Calopeia's demand
# Higher weights indicate a higher emphasis on Year 2 data
# For simple average, set weight = 0.5
average <- function(weight) {
  while (weight < 0 || weight > 1) {
    stop('Weights must be between 0 and 1 inclusive.')
  }
  demand_cal$average <<- demand_cal$year1 * (1 - weight) + demand_cal$year2 * weight
}

average(0.7)

# Plot demand
ggplot(data = melt(demand_cal, id.vars = 'day')) +
  geom_line(aes(x = day, y = value)) +
  facet_wrap(~variable)

ggplot(data = melt(demand_oth, id.vars = 'day')) +
  geom_line(aes(x = day, y = value)) +
  facet_wrap(~variable)
