# A script to collect the Weather data in Forecast.io
# devtools::install_github("hrbrmstr/Rforecastio")
library(dplyr)
library(lubridate)
library(Rforecastio)
options(scipen = 100)

d <- read.csv("data/acc-scot-2005-2012.csv")
d$Date <- dmy(d$Date) %>% as.numeric

# sample 1000 rows and make 1000 queries for free
d <- d %>% sample_n(1000)

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
  d$Date[1],
  exclude = "minutely,hourly,alerts,flags,current"
)$daily
weather[1,] <- NA

# collect data
for(i in 1:nrow(d)) {
  weather[i,] <- getDailyWeather(
    d$Latitude[i], 
    d$Longitude[i], 
    d$Date[i]
  )
  if(i%%10 == 0) cat((i/nrow(d))*100, "% done\n")
}

# rejoin to original weather data
names(weather) <- paste0("WTR_", names(weather))
weather_d <- cbind(d, weather)
write.csv(weather_d, "data/sample_weather.csv", row.names = FALSE)

