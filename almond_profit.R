#' Profit Model for Almond Yield
#' 
#' Calculates profit of almond production adjusting for yield anomalies
#'
#' @param yield_anomaly A number. Difference between actual yield and and baseline yield (ton/acre)
#' @param baseline_yield A number. The typical yield under normal growing conditions (ton/acre)
#' @param almond_price_ton A number. Price of almonds by ton. ($/ton)
#' @param production_cost A number. Base production cost (including fertilizer, labor, land costs, etc) ($/acre)
#' @param anomaly_cost A number. Adjustment for when yield is worse than normal (negative yield_anomaly)
#'
#' @returns A number. Profit from almond yield. Calculated by revenue - total cost. ($/acre)
#'
#' @examples almond_profit (0), almond_profit(-.2)
almond_profit <- function(yield_anomaly,
                          baseline_yield = 0.8,
                          almond_price_ton = 8000,
                          production_cost = 4000,
                          anomaly_cost = 250) {
  
  # First find the actual almond yield
  yield <- baseline_yield + yield_anomaly
  
  # Account for anomaly adjustment in production cost (increase cost when yield is lower)
  total_cost <- ifelse(anomaly_cost < 0,
                       production_cost - (anomaly_cost * yield_anomaly),
                       production_cost)
  
  # Calculate revenue
  revenue <- yield * almond_price_ton
  
  # Calculate profit
  profit <- revenue - total_cost
  
  return(profit)
}