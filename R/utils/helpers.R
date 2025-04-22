# General helper functions

is_monday <- function(date) {
  # Convert the input to Date class if it's not already
  if (!inherits(date, "Date")) {
    date <- as.Date(date)
  }
  
  # Extract weekday (1 = Monday, 2 = Tuesday, ..., 7 = Sunday)
  weekday <- as.integer(format(date, "%u"))
  
  # Check if it's Monday (1)
  return(weekday == 1)
}


