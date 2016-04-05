### does the weather differ on accident days vs non accident days?
library(dplyr)
library(tidyr)
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

se <- function(x) sd(x) / sqrt(length(x))

wthr <- d %>% 
  select(which(isNum(d))) %>%
  select(Date, no_accident_dates, Accident_Severity, matches("WTR")) %>%
  gather(weather_var, value, -c(1:3)) %>%
  separate(weather_var, c("type", "variable"), "_", extra = "merge") %>%
  mutate(variable = sub("WTR_", "", variable)) %>%
  spread(type, value) %>%
  mutate(weather_difference = CONTROL - WTR)

theme_stripped <- theme(
  panel.background = element_blank()
)

summaries <- wthr %>%
  group_by(variable) %>%
  mutate(scaled_weather_diff = scale(weather_difference, F, T)) %>%
  summarise_each(funs(mean, se), scaled_weather_diff)

pdf("plots/weather_difference.pdf", 5, 5)
ggplot(summaries, aes(x = 1, y = mean)) +
  geom_hline(aes(yintercept = 0), col = "grey") +
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se)) +
  facet_wrap(~variable, nrow = 1) + 
  xlab("") + ylab("") +
  theme_stripped +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        strip.text = element_text(angle = 90, vjust = 0))
dev.off()

severity_summaries <- wthr %>%
  group_by(variable, Accident_Severity) %>%
  summarise_each(funs(mean, se), weather_difference)

ggplot(summaries, aes(x = Accident_Severity, y = mean)) +
  geom_line() +
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se)) +
  facet_wrap(~ variable, scales = "free") +
  theme_stripped

