### does adverse weather corrlate with number of incidents on particular days
### some data wrangling
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

d <- read.csv("data/sample_weather_control.csv")

isNum <- function(dd) {
  n <- ncol(dd)
  num_ind <- logical(n)
  for(i in 1:n) {
    num_ind[i] <- is.numeric(dd[,i])
  }
  return(num_ind)
}

accidents <- d[,1:56]
no_accidents <- d[,57:ncol(d)]

names(no_accidents) <- sub("CONTROL_", "", names(no_accidents))
names(no_accidents)[1] <- "Date"

dd <- full_join(accidents, no_accidents)
nrow(dd) == nrow(accidents) + nrow(no_accidents)

dd$accident <- 1
dd$accident[which(is.na(dd$Accident_Index))] <- 0
sum(dd$accident)/nrow(dd)

dd$date <- dd$Date %>%
  as.POSIXct(origin = "1970-01-01") %>%
  as.Date

numeric_wthr_vars <- dd %>%
  select(which(isNum(dd))) %>%
  select(matches("WTR"))

wthr_averages <- cbind(dd %>% select(date), numeric_wthr_vars) %>%
  group_by(date) %>%
  summarise_each(funs(mean), everything())
accident_counts <- dd %>%
  select(date, accident) %>%
  group_by(date) %>%
  summarise(accident_count = sum(accident))

wthr_accidents <- full_join(accident_counts, wthr_averages)
anyNA(wthr_accidents)

write.csv(wthr_accidents, "data/weather_accidents.csv", row.names = FALSE)


