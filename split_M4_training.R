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
m4_info_df <- read_csv("M4-info.csv")

# "1750-01-01 00:00:00"
fix_start <- function(date_str) {
  # 18th Century
  if (nchar(date_str) == 19) {
    return(date_str)
  }

  day_str <- substr(date_str, 1, 2)
  month_str <- substr(date_str, 4, 5)
  year_str <- substr(date_str, 7, 8)

  if (nchar(date_str) == 13) {
    # Single digit hour
    hour_str <- paste0("0", substr(date_str, 10, 10))
    min_str <- substr(date_str, 12, 13)
    # Double digit hour
  } else {
    hour_str <- substr(date_str, 10, 11)
    min_str <- substr(date_str, 13, 14)
  }
  # print(paste(day_str, month_str, year_str, hour_str, min_str))

  if (as.integer(year_str) > 18) {
    # 19th Century
    return(
      paste0(
        "19",
        year_str,
        "-",
        month_str,
        "-",
        day_str,
        " ",
        hour_str,
        ":",
        min_str,
        ":00"
      )
    )
  } else {
    # 20th Century
    return(
      paste0(
        "20",
        year_str,
        "-",
        month_str,
        "-",
        day_str,
        " ",
        hour_str,
        ":",
        min_str,
        ":00"
      )
    )
  }
}

# fix_start_v <- Vectorize(fix_start)
# m4_info_df$fix <- fix_start_v(m4_info_df$StartingDate)

ts_to_json <- function(idx, ts_list, type_list, start_date_list) {
  json <- (paste0(toJSON(
    list(
      start = fix_start(start_date_list[[idx]]),
      target = ts_list[[idx]],
      feat_static_cat = c(idx, as.numeric(type_list[[idx]]))
      # feat_dynamic_real = matrix(rexp(10 * length(ts_list[[idx]])), ncol =
      #                              10)
    ),
    auto_unbox = TRUE
  ), "\n"))
  return(json)
}

process_period <- function(period, m4_data, final_mode) {
  print(period)
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
    lapply(m4_period_data, function(ts)
      return(ts$st))

  m4_start_date <-
    lapply(1:length(m4_st), function(idx)
      return(m4_info_df$StartingDate[m4_info_df$M4id == m4_st[[idx]]]))

  m4_type <-
    lapply(m4_period_data, function(ts)
      return(ts$type))

  m4_horiz <-
    lapply(m4_period_data, function(ts)
      return(ts$h))

  ###########################################################################
  # Create TS depending on final_mode ####

  if (final_mode) {
    dirname <-
      paste0("~/.mxnet_dual_cat_final/gluon-ts/datasets/m4_",
             tolower(period),
             '/')
    m4_train <-
      lapply(m4_period_data, function(ts)
        return(ts$x))

    # train + test
    m4_test <-
      lapply(m4_period_data, function(ts)
        return(c(ts$x, ts$xx)))
  } else {
    dirname <-
      paste0("~/.mxnet_dual_cat_training/gluon-ts/datasets/m4_",
             tolower(period),
             '/')
    m4_train <-
      lapply(m4_period_data, function(ts)
        return(subset(ts$x, end = (
          length(ts$x) - ts$h
        ))))

    # train + test
    m4_test <-
      lapply(m4_period_data, function(ts)
        return(ts$x))
  }

  ###########################################################################
  # Write JSON train and test data ####

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

final_mode <- TRUE
periods <- as.vector(levels(m4_data[[1]]$period))
# periods <- c("Yearly")
res <- unlist(lapply(periods, process_period, m4_data, final_mode))
names(res) <- periods
print(res)
print(sum(res))
