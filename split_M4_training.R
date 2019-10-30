library(M4comp2018)
library(parallel)
library(lubridate)
library(anytime)
library(jsonlite)
library(forecast)
source("benchmarks_eval.R")
library(tidyverse)

set.seed(42)
options(warn = 0)
options(width = 1024)

###########################################################################
# Config ####

if (interactive()) {
  prop_ts <- NA
  num_cores <- 4
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

# Process m4-info start dates
read_csv("M4-info.csv") %>%
  mutate(start_date = parse_date_time(StartingDate, "dmy hs")) %>%
  rename(st = M4id) %>%
  select(st, start_date) ->
  m4_start_date_df

###########################################################################

ts_to_json <- function(idx, ts_list, type_list, start_date_list) {
  json <- (paste0(toJSON(
    list(
      start = start_date_list[[idx]],
      target = ts_list[[idx]],
      feat_static_cat = c(as.numeric(type_list[[idx]]))
      # feat_dynamic_real = matrix(rexp(10 * length(ts_list[[idx]])), ncol =
      #                              10)
    ),
    auto_unbox = TRUE
  ), "\n"))
  return(json)
}

process_period <- function(period, m4_data) {
  print(period)
  dirname <-
    paste0("~/.mxnet_training/gluon-ts/datasets/m4_", tolower(period), '/')

  m4_period_data <- Filter(function(ts)
    ts$period == period, m4_data)

  # len_m4_period <-
  #   unlist(lapply(m4_period_data, function(ts)
  #     return(length(ts$x))))
  # print(ggplot(tibble(ts_length = len_m4_period)) + geom_histogram(aes(x = ts_length), bins=100) + ggtitle(period) + scale_x_log10())

  # if (use_parallel) {
  #   m4_data_x_deseason <- mclapply(1:length(m4_data_x), function(idx)
  #     return(deseasonalise(m4_data_x[[idx]], m4_horiz[[idx]])), mc.cores = num_cores)
  # } else {
  #   m4_data_x_deseason <- lapply(1:length(m4_data_x), function(idx)
  #     return(deseasonalise(m4_data_x[[idx]], m4_horiz[[idx]])))
  # }

  m4_st <-
    unlist(lapply(m4_period_data, function(ts)
      return(ts$st)))

  m4_start_date <-
    lapply(m4_st, function(st)
      return(m4_start_date_df$start_date[m4_start_date_df$st == st]))

  m4_type <-
    lapply(m4_period_data, function(ts)
      return(ts$type))

  m4_horiz <-
    lapply(m4_period_data, function(ts)
      return(ts$h))

  m4_train <-
    lapply(m4_period_data, function(ts)
      return(subset(ts$x, end = (
        length(ts$x) - ts$h
      ))))

  # train + test
  m4_test <-
    lapply(m4_period_data, function(ts)
      return(ts$x))

  json <-
    lapply(1:length(m4_train),
           ts_to_json,
           m4_train,
           m4_type,
           m4_start_date)
  sink(paste0(dirname, "train/data.json"))
  lapply(json, cat)
  sink()

  json <-
    lapply(1:length(m4_test),
           ts_to_json,
           m4_test,
           m4_type,
           m4_start_date)
  sink(paste0(dirname, "test/data.json"))
  lapply(json, cat)
  sink()

  return(length(m4_train))
}

# periods <- as.vector(levels(m4_data[[1]]$period))
periods <- c("Hourly")
res <- unlist(lapply(periods, process_period, m4_data))
names(res) <- periods
print(res)
print(sum(res))
