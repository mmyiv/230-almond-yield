#' Profit Model for Almond Yield
#' 
#' Calculates profit of almond production adjusting for yield anomalies
#'
#' @param yield_anomaly A number. Difference between actual yield and and baseline yield (ton/acre)
#' @param baseline_yield A number. The typical yield under normal growing conditions (ton/acre)
#' @param price A number. Price of almonds by ton. ($/ton)
#' @param production_cost A number. Base production cost (including fertilizer, labor, land costs, etc) ($/acre)
#'
#' @returns A number. Profit from almond yield. Calculated by revenue - total cost. ($/acre)
#'
#' @examples almond_profit (0), almond_profit(-.2)
almond_profit <- function(yield_anomaly,
                          baseline_yield = 0.8,
                          price = 5000,
                          production_cost = 4000) {
  
  # Extract February minimum temperature and January precipitation
  feb_min_t <- daily_data %>%
    filter(month == 2) %>%
    group_by(year) %>%
    summarize(feb_min_t = min(tmin_c, na.rm = TRUE)) %>% ungroup()
  
  jan_precip <- daily_data %>%
    filter(month == 1) %>%
    group_by(year) %>%
    summarize(jan_p = sum(precip, na.rm = TRUE)) %>% ungroup()
  
  # Merge February temperature and January precipitation by year
  climate_data <- left_join(feb_min_t, jan_precip, by = "year")
  
  # Compute yield anomaly for each year
  climate_data <- climate_data %>%
    
    mutate(yield_anomaly = -0.015 * feb_min_t + -0.0046 * feb_min_t^2 + -0.07 * jan_p +  0.0043 * jan_p^2 + 0.28,
           yield = baseline_yield + yield_anomaly,
           revenue = yield * price,
           profit = revenue - production_cost)
  
  results <- climate_data %>%
    select(year, yield, profit, yield_anomaly) %>%
    mutate(mean_yield_anomaly = mean(yield_anomaly, na.rm = TRUE),
           mean_profit = mean(profit, na.rm = TRUE))
  
  return(results)
}
