library(M4comp2018)
# library(parallel)
library(lubridate)
library(tidyverse)

set.seed(42)
options(warn = 2)
options(width = 1024)
source("fcast.R")

###########################################################################
# Config ####

m4_freqs <- read_csv("m4_horiz.csv")
horizons <- as.list(m4_freqs$Horizon)
names(horizons) <- m4_freqs$SP

if (interactive()) {
  prop_ts <- 0.01
  num_cores <- 2
} else
{
  prop_ts <- NA
  num_cores <- 16
}
use_parallel <- TRUE #is.na(prop_ts)

###########################################################################
# Preprocess M4 data ####

# M4 <- Filter(function(ts)
#   ts$period == "Quarterly" | ts$period == "Monthly", M4)

if (is.na(prop_ts)) {
  m4_data <- M4
} else {
  m4_data <- sample(M4, prop_ts * length(M4))
}

m4_data_x <-
  lapply(m4_data, function(ts)
    return(ts$x))

m4_period <-
  lapply(m4_data, function(ts)
    return(ts$period))

m4_horiz <-
  lapply(1:length(m4_data_x), function(idx)
    return(horizons[[as.character(m4_period[[idx]])]]))

m4_type <-
  lapply(m4_data, function(ts)
    return(ts$type))

###########################################################################
# Split M4 data ####

periods <- as.vector(levels(m4_data[[1]]$period))

m4_train <-
  lapply(1:length(m4_data), function(idx)
    return(subset(m4_data_x[[idx]], end = length(m4_data_x[[idx]]) - m4_horiz[[idx]])))
m4_test <-
  lapply(1:length(m4_data), function(idx)
    return(subset(m4_data_x[[idx]], start = (
      length(m4_data_x[[idx]]) - m4_horiz[[idx]] + 1
    ))))

write_ts <- function(train, test, type) {
  ts_list <- lapply()
}
