### a script to obtain baseline weather data
### this will be randomly sampled weather points within time limits

library(dplyr)
library(lubridate)
library(Rforecastio)
options(scipen = 100)

d <- read.csv("data/sample_weather.csv")

# match each row with a random date within the range
# that is at least three days away from the day of the accident
date_range <- range(d$Date)
date_vec <- date_range[1]:date_range[2]

d$no_accident_dates <- integer(nrow(d))
three_days <- 60*60*24*3
for(i in 1:nrow(d)) {
  d$no_accident_dates[i] <- date_vec[sample.int(length(date_vec), 1)]
  date_dist <- abs(d$no_accident_dates[i] - d$Date[i])
  while(date_dist < three_days){
    d$no_accident_dates[i] <- date_vec[sample.int(length(date_vec), 1)]
    date_dist <- abs(d$no_accident_dates[i] - d$Date[i])
  }
  print(i)
}

# quality check
dists <- no_accident_dates - d$Date
any(abs(dists) < three_days)


# Connect session to API
Sys.setenv(FORECASTIO_API_KEY = "52e3ce497f1e9d184bbf8edc5a46c72c") 
# or replace with your own quoted API key
forecastio_api_key()

# set up data collection
getDailyWeather <- function(lat, lon, time) {
  this <- get_forecast_for(
    lat, lon, time,
    exclude = "minutely,hourly,currently,alerts,flags"
  )
  return(this$daily)
}
weather <- get_forecast_for(
  d$Latitude[1], 
  d$Longitude[1], 
  d$no_accident_dates[1],
  exclude = "minutely,hourly,alerts,flags,current"
)$daily
weather[1,] <- NA

# collect data
for(i in 1:nrow(d)) {
  weather[i,] <- getDailyWeather(
    d$Latitude[i], 
    d$Longitude[i], 
    d$no_accident_dates[i]
  )
  if(i%%10 == 0) cat((i/nrow(d))*100, "% done\n")
}

# rejoin to original weather data
names(weather) <- paste0("CONTROL_WTR_", names(weather))
weather_d <- cbind(d, weather)
write.csv(weather_d, "data/sample_weather_control.csv", row.names = FALSE)
