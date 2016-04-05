### does adverse wethear corrlate with number of incidents on particular days
### a random forest test
library(dplyr)
library(randomForest)
d <- read.csv("data/weather_accidents.csv")

dtrain <- d %>% select(matches("WTR"))

y <- character(nrow(d))

y[d$accident_count == 0] <- "no"
y[d$accident_count > 0] <- "yes"

fit <- randomForest(
  x = dtrain,
  y = as.factor(y),
  ntree = 1000,
  mtry = 12,
  do.trace = TRUE
)

varImpPlot(fit)

cv <- rfcv(trainx = dtrain, trainy = as.factor(y))
cv$error.cv
