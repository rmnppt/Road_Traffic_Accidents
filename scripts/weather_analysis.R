library(dplyr)
library(randomForest)

d <- read.csv("data/sample_weather.csv")

applyDD <- function(d, func) {
  v <- logical(ncol(d))
  for(i in 1:ncol(d)) {
    v[i] <- do.call(func, list(d[,i]))
  }
  return(v)
}
factorz <- applyDD(d, "is.factor")
high_levels <- applyDD(d, function(x) length(levels(x)) > 20)
remove <- factorz & high_levels

predictors <- d %>% select(-which(remove), 
                           -Accident_Severity, 
                           -LSOA_of_Accident_Location)

weather_predictors <- predictors %>% 
  select(matches("WTR"))
traffic_predictors <- predictors %>%
  select(-matches("WTR"))

response <- d$Accident_Severity
response[response == 1 | response == 2] <- 0
response[response == 3] <- 1
response <- as.factor(response)

fit <- randomForest(
  x = predictors,
  y = response,
  ntree = 2000,
  mtry = 3,
  # nodesize = 2,
  # maxnodes = 10,
  na.action = na.omit
)
fit

varImpPlot(fit)

cv_weather  <- rfcv(
  trainx = traffic_predictors,
  trainy = response,
  cv.fold = 2,
  ntree = 1000,
  na.action = na.omit
)

cv_traffic <- rfcv(
  trainx = weather_predictors,
  trainy = response,
  cv.fold = 2,
  ntree = 1000,
  na.action = na.omit
)
