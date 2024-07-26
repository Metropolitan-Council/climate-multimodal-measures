pedestrian_facilities <- function(average_daily_traffic, no_key_destinations, project_start, project_lifetime) {
  annual_use_days <- 214
  average_trip_replaced <- 0.6 #in miles
  
  mode_shift_factor <- ModeShiftFactor %>% filter(average_daily_traffic_vehicle_trips_per_day == average_daily_traffic)
  
}