library(dbplyr)
library(ggplot2)

library(decisionSupport)

# Load necessary package
library(decisionSupport)


# ---- STEP 1: SAMPLE VARIABLES FOR TESTING ----
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (i in colnames(x)) assign(i, as.numeric(x[1, i]), envir = .GlobalEnv)
}
make_variables(input)

make_variables(estimate_read_csv(paste("C:/Users/Mein PC/OneDrive/Brown/Research project/Decision analysis/cocoa_maize.csv",
                                       sep = "")))

# ---- STEP 3: DECISION MODEL FUNCTION ----
cocoa_maize_model <- function(x, varnames) {
  with(as.list(x), {
    
    n_years <- as.numeric(n_years)  # <--- explicitly ensure it's numeric
    
    # Initialize vectors
    maize_revenue_mono <- rep(0, n_years)
    maize_revenue_af <- rep(0, n_years)
    cocoa_revenue <- rep(0, n_years)
    
    af_cost <- rep(op_cost_af, n_years)
    af_cost[1] <- af_cost[1] + est_cost_af
    mono_cost <- rep(op_cost_mono, n_years)
    
    # Simulate risks
    maize_risk_event <- chance_event(maize_risk, value_if = 1, n = n_years)
    cocoa_risk_event <- chance_event(cocoa_risk, value_if = 1, n = n_years)
    
    for (i in 1:n_years) {
      maize_yield_m <- vv(maize_yield_mono * (1 - maize_risk_event[i] * yield_maize_risk), 0.1, 1)
      maize_yield_af_val <- vv(maize_yield_af * (1 - maize_risk_event[i] * yield_maize_risk), 0.1, 1)
      cocoa_yield_val <- ifelse(i >= 4, vv(cocoa_yield_af * (1 - cocoa_risk_event[i] * yield_cocoa_risk), 0.2, 1), 0)
      
      maize_revenue_mono[i] <- maize_yield_m * maize_price
      maize_revenue_af[i] <- maize_yield_af_val * maize_price
      cocoa_revenue[i] <- cocoa_yield_val * cocoa_price
    }
    
    total_af_benefit <- (maize_revenue_af + cocoa_revenue) - af_cost
    total_mono_benefit <- maize_revenue_mono - mono_cost
    tradeoff <- total_af_benefit - total_mono_benefit
    
    NPV_af <- discount(total_af_benefit, discount_rate)
    NPV_mono <- discount(total_mono_benefit, discount_rate)
    NPV_tradeoff <- discount(tradeoff, discount_rate)
    
    return(list(
      NPV_af = NPV_af,
      NPV_mono = NPV_mono,
      tradeoff = NPV_tradeoff
    ))
  })
}

# ---- STEP 4: RUN MONTE CARLO SIMULATION ----
decisionSupport(
  inputFilePath = "C:/Users/Mein PC/OneDrive/Brown/Research project/Decision analysis/cocoa_maize.csv",  # <- path to your CSV file
  welfareFunction = cocoa_maize_model,
  numberOfModelRuns = 10000,
  write_table = TRUE,
  outputPath = "MCResults/cocoa_maize"
)


