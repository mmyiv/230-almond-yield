#' Applied Profit and Yield calculation for Almond Production
#'
#' @param daily_data. A dataframe containing daily climate dataset
#' @param Tn1 Coefficient for linear term of minimum temperature (default: -0.015)
#' @param Tn2 Coefficient for quadratic term of minimum temperature (default: -0.0046)
#' @param P1 Coefficient for linear term of precipitation (default: -0.07)
#' @param P2 Coefficient for quadratic term of precipitation (default: 0.0043)
#' @param i Intercept term (default: 0.28)
#' 
#' @param baseline_yield A number. The typical yield under normal growing conditions (ton/acre)
#' @param almond_price_ton A number. Price of almonds by ton. ($/ton)
#' @param production_cost A number. Base production cost (including fertilizer, labor, land costs, etc) ($/acre)
#' @param anomaly_cost A number. Adjustment for when yield is worse than normal (negative yield_anomaly)
#'
#' @returns Almond minimum, maximum, and mean yield and profit as tons/acre or $/acre
#'
applied_profit_yield <- function(daily_data, 
                                 Tn1 = -0.015, 
                                 Tn2 = -0.0046, 
                                 P1 = -0.07, 
                                 P2 = 0.0043, 
                                 i = 0.28,
                                 baseline_yield = 0.8,
                                 almond_price_ton = 8000, 
                                 production_cost = 4000, 
                                 anomaly_cost = 250) {
  
  # Calculate almond yield and yield anomalies
  
    # Extract February minimum temperature 
    feb_min_t <- daily_data %>%
      filter(month == 2) %>%
      group_by(year) %>%
      summarize(feb_min_t = min(tmin_c, na.rm = TRUE)) %>%
      ungroup()
    
    # Extract January precipitation
    jan_precip <- daily_data %>%
      filter(month == 1) %>%
      group_by(year) %>%
      summarize(jan_p = sum(precip, na.rm = TRUE)) %>%
      ungroup()
  
    # Merge February temperature and January precipitation by year
    climate_data <- left_join(feb_min_t, jan_precip, by = "year")
  
    # Compute yield and yield anomaly for each year
    climate_data <- climate_data %>%
      mutate(yield = Tn1 * feb_min_t + Tn2 * feb_min_t^2 + P1 * jan_p + P2 * jan_p^2 + i,
             yield_anomaly = yield - baseline_yield)
  
  # Calculate profit using yield anomalies ------------------------------
    climate_data <- climate_data %>%
      mutate(profit = almond_profit(yield_anomaly, baseline_yield, almond_price_ton, production_cost, anomaly_cost))
  return(climate_data %>%
            select(year, yield_anomaly, profit))
    
    
    # # Calculate summary stats
    # summary_stats <- climate_data %>%
    #   summarise(
    #     max_yield = max(yield, na.rm = TRUE),
    #     min_yield = min(yield, na.rm = TRUE),
    #     mean_yield = mean(yield, na.rm = TRUE),
    #     max_profit = max(profit, na.rm = TRUE),
    #     min_profit = min(profit, na.rm = TRUE),
    #     mean_profit = mean(profit, na.rm = TRUE)
    #   )
    # 
    # return(list(
    #   max_yield = summary_stats$max_yield,
    #   min_yield = summary_stats$min_yield,
    #   mean_yield = summary_stats$mean_yield,
    #   max_profit = summary_stats$max_profit,
    #   min_profit = summary_stats$min_profit,
    #   mean_profit = summary_stats$mean_profit))
      
}

