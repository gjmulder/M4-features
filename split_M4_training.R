library(M4comp2018)
# library(parallel)
library(lubridate)
library(tidyverse)
library(jsonlite)

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
  prop_ts <- NA
  num_cores <- 2
} else
{
  prop_ts <- NA
  num_cores <- 16
}
use_parallel <- TRUE #is.na(prop_ts)

###########################################################################
# Preprocess M4 data ####

period <- "Yearly"
M4 <- Filter(function(ts)
  ts$period == period, M4)

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


m4_train <-
  lapply(1:length(m4_data), function(idx)
    return(subset(m4_data_x[[idx]], end = length(m4_data_x[[idx]]) - m4_horiz[[idx]])))
m4_test <-
  lapply(1:length(m4_data), function(idx)
    return(subset(m4_data_x[[idx]], start = (
      length(m4_data_x[[idx]]) - m4_horiz[[idx]] + 1
    ))))

ts_to_json <- function(idx, ts, type) {
  return(paste0(toJSON(
    list(
      start = "1750-01-01 00:00:00",
      target = ts[[idx]],
      feat_static_cat = c(as.numeric(type[[idx]]))
    ),
    auto_unbox = TRUE
  ), "\n"))
}

write_json <- function(period, train, test, type) {
  dirname <-
    paste0("~/.mxnet/gluon-ts/datasets/m4_", tolower(period), '/')
  json <- lapply(1:length(train), ts_to_json, train, type)
  sink(paste0(dirname, "train/data.json"))
  lapply(json, cat)
  sink()

  json <- lapply(1:length(test), ts_to_json, test, type)
  sink(paste0(dirname, "test/data.json"))
  lapply(json, cat)
  sink()
  return(length(test))
}

# periods <- as.vector(levels(m4_data[[1]]$period))
lapply(period, write_json, m4_train, m4_test, m4_type)
