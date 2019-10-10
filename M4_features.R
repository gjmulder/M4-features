library(M4comp2018)
library(tsfeatures)
library(parallel)
# library(GGally)
library(gridExtra)
# library(ggplot2)
library(tidyverse)

set.seed(42)
# options(warn = 2, width = 1024)
source("fcast.R")

###########################################################################
# Config ####

if (interactive()) {
  prop_ts <- NA #0.01
} else
{
  prop_ts <- NA
}
use_parallel <- is.na(prop_ts)
m4_freqs <- read_csv("m4_horiz.csv")
horizons <- as.list(m4_freqs$Horizon)
names(horizons) <- m4_freqs$SP
err_names <- c("sMAPE", "MASE")

###########################################################################
# Preprocess M4 data ####

if (is.na(prop_ts)) {
  m4_data <- M4
} else {
  # m4_data <- Filter(function(ts)
  #   ts$period == "Monthly", M4)
  m4_data <- sample(M4, prop_ts * length(M4))
}

m4_data_x <-
  lapply(m4_data, function(ts)
    return(ts$x))

m4_type <-
  lapply(m4_data, function(ts)
    return(ts$type))
names(m4_type) <- "type"

m4_period <-
  lapply(m4_data, function(ts)
    return(ts$period))
names(m4_period) <- "period"

m4_horiz <-
  lapply(1:length(m4_data_x), function(idx)
    return(horizons[[as.character(m4_period[[idx]])]]))

if (use_parallel) {
  m4_data_x_deseason <- mclapply(1:length(m4_data_x), function(idx)
    return(deseasonalise(m4_data_x[[idx]], m4_horiz[[idx]])), mc.cores = 16)
} else {
  m4_data_x_deseason <- lapply(1:length(m4_data_x), function(idx)
    return(deseasonalise(m4_data_x[[idx]], m4_horiz[[idx]])))
}
m4_data_xx <-
  lapply(m4_data, function(ts)
    return(ts$xx))

###########################################################################
# Forecast each TS using each of the benchmark methods ####

print("M4 Competition data:")

if (use_parallel) {
  fcasts <- mclapply(
    1:length(m4_data_x),
    multi_fit_ts,
    m4_data_x,
    m4_data_x_deseason,
    m4_horiz,
    mc.cores = 16
  )
} else {
  fcasts <- lapply(1:length(m4_data_x),
                   multi_fit_ts,
                   m4_data_x,
                   m4_data_x_deseason,
                   m4_horiz)
}
fcast_names <- names(fcasts[[1]])

###########################################################################
# Compute sMAPE and MASE ####

if (use_parallel) {
  fcast_errs <- mclapply(1:length(fcasts),
                         compute_fcast_errs,
                         fcasts,
                         m4_data_x,
                         m4_data_xx,
                         mc.cores = 16)
} else {
  fcast_errs <- lapply(1:length(fcasts),
                       compute_fcast_errs,
                       fcasts,
                       m4_data_x,
                       m4_data_xx)
}

# fcast_smapes_df <-
#   bind_rows(lapply(fcast_errs,
#                    function(errs)
#                      return(errs[1, ])))
# colnames(fcast_smapes_df) <-
#   paste0(colnames(fcast_smapes_df), "_smape")

fcast_mases_df <-
  bind_rows(lapply(fcast_errs,
                   function(errs)
                     return(errs[2, ])))
# colnames(fcast_mases_df) <-
#   paste0(colnames(fcast_mases_df), "_mase")

###########################################################################
# Features ####

m4_data_proc <- purrr::map(m4_data,
                           function(x) {
                             tspx <- tsp(x$x)
                             ts(c(x$x, x$xx), start = tspx[1], frequency = tspx[3])
                           })
khs_stl <- function(x, ...) {
  lambda <- BoxCox.lambda(x,
                          lower = 0,
                          upper = 1,
                          method = 'loglik')
  y <- BoxCox(x, lambda)
  c(stl_features(y, s.window = 'periodic', robust = TRUE, ...),
    lambda = lambda)
}

khs <-
  bind_cols(
    tsfeatures(m4_data_proc, c("frequency", "entropy"), parallel = use_parallel),
    tsfeatures(
      m4_data_proc,
      "khs_stl",
      scale = FALSE,
      parallel = use_parallel
    )
  ) %>%
  select(frequency, entropy, trend, seasonal_strength, e_acf1, lambda) %>%
  replace_na(list(seasonal_strength = 0)) %>%
  rename(
    Frequency = frequency,
    Entropy = entropy,
    Trend = trend,
    Season = seasonal_strength,
    ACF1 = e_acf1,
    Lambda = lambda
  ) %>%
  mutate(Period = as.factor(Frequency))

###########################################################################
# Combine forecasts and features ####

m4_data_all_df <-
  bind_cols(khs, fcast_mases_df)
m4_data_all_df$type <- unlist(m4_type)
m4_data_all_df$period <- unlist(m4_period)

m4_data_all_df %>%
  group_by(type, period) %>%
  # summarise_if(is.numeric, list(mean=mean, stddev=sd), na.rm = TRUE) ->
  summarise_if(is.numeric, list(mean=mean, sd=sd), na.rm = TRUE) ->
  m4_data_sum_df

m4_data_sum_df %>%
  mutate(
    data = sprintf(
      # "%8.3f%8.3f\n%8.3f%8.3f\n%8.3f%8.3f\n%8.3f%8.3f\n%8.3f%8.3f\n%8.3f%8.3f",
      "%8.3f%8.3f\n%8.3f%8.3f",
      Entropy_mean,
      Entropy_sd,
      # Trend_mean,
      # Trend_sd,
      # Season_mean,
      # Season_sd,
      # ACF1_mean,
      # ACF1_sd,
      # Lambda_mean,
      # Lambda_sd,
      combined_mean,
      combined_sd
    )
  ) %>%
  select(type, period, data) %>%
  spread(type, data) %>%
  select(2:ncol(.), 1) ->
  results_df

col_names <-  paste0(colnames(results_df), "\nmean   sd")
col_names[length(col_names)] <- " "

# png('feat_freqs.png', width = 4096, height = 4096)
dev.off()
# print(grid.table(results_df, rows = rep("Entropy\nTrend\nSeason\nACF1\nLambda\nMASE", nrow(results_df)), cols = col_names))
print(grid.table(results_df, rows = rep("Entropy\nMASE", nrow(results_df)), cols = col_names))
