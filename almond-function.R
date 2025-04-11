#' @title Almond Yield
#' @description Computes almond yield, taking daily data and providing yearly averaged results
#'
#' @param Tn1 Coefficient for linear term of minimum temperature (default: -0.015)
#' @param Tn2 Coefficient for quadratic term of minimum temperature (default: -0.0046)
#' @param P1 Coefficient for linear term of precipitation (default: -0.07)
#' @param P2 Coefficient for quadratic term of precipitation (default: 0.0043)
#' @param i Intercept term (default: 0.28)
#' @param daily_data Provided climate dataset
#'
#' @author Bailey Jørgensen and Michelle Yiv for EDS230 with Naomi Tague
#' @references D.B. Lobell et al. 2006.
#' @return Almond yield from California: mean, maximum, minimum yields. Units: tons per acre.
#'
almond_yield <- function(daily_data, Tn1 = -0.015, Tn2 = -0.0046, P1 = -0.07, P2 = 0.0043, i = 0.28) {
  
  # Extract February minimum temperature and January precipitation
  feb_min_t <- daily_data %>%
    filter(month == 2) %>%
    group_by(year) %>%
    summarize(feb_min_t = min(tmin_c, na.rm = TRUE), .groups = "drop")
  
  jan_precip <- daily_data %>%
    filter(month == 1) %>%
    group_by(year) %>%
    summarize(jan_p = sum(precip, na.rm = TRUE), .groups = "drop")
  
  # Merge February temperature and January precipitation by year
  climate_data <- left_join(feb_min_t, jan_precip, by = "year")
  
  # Compute yield for each year
  climate_data <- climate_data %>%
    mutate(yield = Tn1 * feb_min_t + Tn2 * feb_min_t^2 + P1 * jan_p + P2 * jan_p^2 + i)
  
  # Return max, min, and mean yields
  return(list(
    maxyield = max(climate_data$yield, na.rm = TRUE),
    minyield = min(climate_data$yield, na.rm = TRUE),
    meanyield = mean(climate_data$yield, na.rm = TRUE)
  ))
}