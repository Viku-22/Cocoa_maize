library(dbplyr)
library(ggplot2)

library(decisionSupport)

# Cocoa and Maize #####

num_simulation <- 10000

# maize yield (from 250kg - 500kg per year) with average expenditure at (176.60usd
# - 441.50usd) and market price of 0.96 usd per kg
250*0.96

maize_income_range <- c(240,480)
maize_cost_range <- c(176.60,441.50)


maize_income <- runif(n=num_simulation, min = maize_income_range[1],
                      max = maize_income_range [2])

maize_cost <- runif(n=num_simulation, min = maize_cost_range[1],
                    max = maize_cost_range [2])

maize_profits <- maize_income - maize_cost

hist(maize_profits)


# cocoa yield (from 300kg - 400kg per hectare) with average expenditure at (1000usd
# - 3000usd) and market price of 7.35usd - 10.5usd usd per kg

(300*7.35)+(300*10.5)/2  
(400*7.35)+(400*10.5)/2
cocoa_income_range <- c(3780,5040)
cocoa_cost_range <- c(1000,3000)


cocoa_income <- runif(n=num_simulation, min = cocoa_income_range[1],
                      max = cocoa_income_range [2])

cocoa_cost <- runif(n=num_simulation, min = cocoa_cost_range[1],
                    max = cocoa_cost_range [2])

cocoa_profits <- cocoa_income - cocoa_cost

hist(cocoa_profits)


# cocoa-maize farm
