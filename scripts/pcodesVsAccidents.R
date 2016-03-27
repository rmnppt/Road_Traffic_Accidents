# correlate post code density and accidents
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

acc <- read_csv("data/acc-scot-2005-2012.csv")
pcode <- read_csv("data/postcodes.csv")

# the data needs to be alligned in both DF's for later use in calcHex()
accRef <- data.frame(
  x = acc$Location_Easting_OSGR,
  y = acc$Location_Northing_OSGR
)
pcodeRef <- data.frame(
  x = pcode$x,
  y = pcode$y
)
accRef$group <- "accident"
pcodeRef$group <- "postcode"

# might be some difficulty with consistant 
# binning over data with different resolution
pcodeRef <- as.data.frame(apply(pcodeRef, 2, signif, digits = 5))

# useful to have the range of the full data
xRange <- c(
  min(c(accRef$x, pcodeRef$x), na.rm = T),
  max(c(accRef$x, pcodeRef$x), na.rm = T)
)
yRange <- c(
  min(c(accRef$y, pcodeRef$y), na.rm = T),
  max(c(accRef$y, pcodeRef$y), na.rm = T)
)



dat <- rbind(pcodeRef, accRef)

# initial calculation of hex's
firstHex <- function(dat, xRange, yRange) {
  p <- dat %>% group_by(group) %>%
  ggplot(aes(x = x, y = y)) +
    geom_bin2d(aes(xmax = xRange[2], xmin = xRange[1], 
                   ymax = yRange[2], ymin = yRange[1]),
               binwidth = c(1000, 1000)) +
    ylim(yRange[1], yRange[2]) +
    xlim(xRange[1], xRange[2])
  ggplot_build(p)
}
combinedBins <- firstHex(dat, xRange, yRange)$data[[1]]
matchedBins <- dcast(combinedBins, count ~ group + )

# reproducible calculation of hex's
calcHex <- function(dat, xmax, xmin, ymax, ymin) {
  p <- ggplot(dat, aes(x = x, y = y)) +
    geom_bin2d(aes(xmax = xmax, xmin = xmin, 
                   ymax = ymax, ymin = ymin))
  ggplot_build(p)
}

# calculate and join
accBins <- calcHex(accRef, xmax, xmin, ymax, ymin)
pcodeBins <- calcHex(pcodeRef, xmax, xmin, ymax, ymin)
accBins$class <- "accident"
pcodeBins$class <- "postcode"
dat <- inner_join(accBins$data[[1]], pcodeBins$data[[1]], 
                  by = c("ymin", "xmin", "ymax", "xmax"))

# relationship between accident density and post code density
# cor.test(log(dat$count.x), log(dat$count.y))
# ggplot(dat, aes(x = log(count.x), y = log(count.y))) +
#   geom_point()

cor.test(dat$count.x, dat$count.y)
ggplot(dat, aes(x = count.x, y = count.y)) +
  geom_point()

