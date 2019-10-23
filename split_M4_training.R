library(M4comp2018)
library(parallel)
library(lubridate)
library(tidyverse)
library(jsonlite)
library(forecast)

set.seed(42)
options(warn = 2)
options(width = 1024)

###########################################################################
# Config ####

if (interactive()) {
  prop_ts <- NA
  num_cores <- 6
} else
{
  prop_ts <- NA
  num_cores <- 16
}
use_parallel <- TRUE #is.na(prop_ts)

###########################################################################
# Preprocess M4 data ####

if (is.na(prop_ts)) {
  m4_data <- M4
} else {
  m4_data <- sample(M4, prop_ts * length(M4))
}

ts_to_json <- function(idx, dat, typ) {
  json <- (paste0(toJSON(
    list(
      start = "1750-01-01 00:00:00",
      target = dat[[idx]],
      feat_static_cat = c(as.numeric(typ[[idx]]))
    ),
    auto_unbox = TRUE
  ), "\n"))
  return(json)
}

process_period <- function(period, m4_data) {
  print(period)
  dirname <-
    paste0("~/.mxnet/gluon-ts/datasets/m4_", tolower(period), '/')

  m4_period_data <- Filter(function(ts)
    ts$period == period, m4_data)

  m4_type <-
    lapply(m4_period_data, function(ts)
      return(ts$type))

  m4_train <-
    lapply(m4_period_data, function(ts)
      return(subset(ts$x, end = (length(ts$x) - ts$h))))

  json <- lapply(1:length(m4_train), ts_to_json, m4_train, m4_type)
  sink(paste0(dirname, "train/data.json"))
  lapply(json, cat)
  sink()

  m4_test <-
    lapply(m4_period_data, function(ts)
      return(ts$x))

  json <- lapply(1:length(m4_test), ts_to_json, m4_test, m4_type)
  sink(paste0(dirname, "test/data.json"))
  lapply(json, cat)
  sink()

  return(length(m4_test))
}

periods <- as.vector(levels(m4_data[[1]]$period))
res <- unlist(lapply(periods, process_period, M4))
names(res) <- periods
print(res)
print(sum(res))
