
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)

dat <- read.csv("data/acc-scot-2005-2012.csv")

theme_stripped <- theme(
  panel.background = element_blank()
)

# condense the sub 20 crashes
dat$Speed_limit[dat$Speed_limit < 20] <- 20
dat$Speed_limit <- paste0(dat$Speed_limit, " mph")

# related to speed limit
p1 <- ggplot(dat, aes(x = as.character(Speed_limit))) +
  geom_bar() +
  theme_stripped +
  xlab("speed limit")

# over time for different speed limits
p2 <- ggplot(dat, aes(x = Calendar_Year)) + 
  geom_point(stat = "bin", binwidth = 1) +
  geom_line(stat = "bin", binwidth = 1) +
  facet_wrap(~ Speed_limit, scales = "free_y") +
  xlim(range(dat$Calendar_Year)) +
  xlab("") + ylab("") +
  theme_stripped +
  theme(axis.text.x = element_text(
    angle = 270, hjust = 0, vjust = 0.5
  ))

pdf("plots/speed_limit.pdf", 10, 5)
grid.arrange(p1, p2, ncol = 2)
dev.off()

# across the map
pdf("plots/rta_map.pdf", 12, 6)
ggplot(dat, aes(x = Longitude, y = Latitude)) +
  geom_point(alpha = 0.25, size = 0.1) +
  ylim(54.5, 60.5) +
  coord_map() +
  facet_wrap(~ Speed_limit, nrow = 1) +
  theme_stripped
dev.off()

# time of day?
pdf("plots/time_of_day.pdf", 10, 5)
dat$hms <- as.POSIXct(hms(dat$Time), origin = "2000-01-01")
ggplot(dat, aes(x = hms)) +
  geom_histogram(bins = 24) +
  scale_x_datetime(date_breaks = "5 hours", 
                   date_labels = "%H:00") +
  facet_wrap(~Speed_limit, scales = "free_y") +
  theme_stripped
dev.off()

# month?
monthz <- months(dat$Time)
ggplot(dat, aes(x = Month)) +
  geom_histogram(aes(fill = as.factor(Accident_Severity)), 
                 bins = 50, alpha = 0.75)

