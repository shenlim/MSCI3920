# Supply Chain Game 2
library(reshape2)
library(ggplot2)

# Load historical demand data
# Full, 730-day historical data is available for Calopeia
# Historical data for other regions only available >= day 640
demand <- read.csv('https://raw.githubusercontent.com/shenlim/MSCI3920/master/demand.csv')

demand_oth <- data.frame(demand[640:nrow(demand), names(demand) != 'Calopeia'])

# Crude forecast for Calopeia's demand
# Higher weights indicate a higher emphasis on Year 2 data
# For simple average, set weight = 0.5
average <- function(weight) {
  while (weight < 0 || weight > 1) {
    stop('Weights must be between 0 and 1 inclusive.')
  }
  
  demand_cal <<- data.frame(demand[, c('day', 'Calopeia')])
  
  for (i in seq(nrow(demand_cal) / 2)) {
    demand_cal[nrow(demand_cal) + 1, ] <<-
      list(i + 730, demand_cal[i, 2] * (1 - weight) + demand_cal[i + 365, 2] * weight)
  }
}

average(0.7)

# Plot demand
plot_cal <- ggplot(data = melt(demand_cal, id.vars = 'day')) +
  geom_line(aes(x = day, y = value)) +
  facet_wrap(~variable) +
  ylab('demand')

plot_oth <- ggplot(data = melt(demand_oth, id.vars = 'day')) +
  geom_line(aes(x = day, y = value)) +
  facet_wrap(~variable) +
  ylab('demand')
