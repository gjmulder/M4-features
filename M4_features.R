library(M4comp2018)
library(tsfeatures)
library(tidyverse)
library(ggplot2)
# library(data.table)

set.seed(42)
options(warn = 2, width = 1024)
source("fcast.R")

###########################################################################
# Config ####

prop_ts <- 0.002
m4_freqs <- read_csv("m4_horiz.csv")
horizons <- as.list(m4_freqs$Horizon)
names(horizons) <- m4_freqs$SP

###########################################################################
# Preprocess M4 data ####

if (is.na(prop_ts)) {
  m4_data <- M4
} else {
  m4_data <- sample(M4, prop_ts * length(M4))
}

# M4 Competition data
m4_data_x <-
  lapply(m4_data, function(ts)
    return(ts$x))

m4_period <-
  lapply(m4_data, function(ts)
    return(ts$period))

m4_data_x_horiz <-
  lapply(1:length(m4_data_x), function(idx) return(horizons[[as.character(m4_period[[idx]])]]))

m4_data_x_deseason <-
  lapply(1:length(m4_data_x), function(idx) return(deseasonalise(m4_data_x[idx], m4_data_x_horiz[[idx]])))


###########################################################################
# Features ####

m4_feat_df <- tsfeatures(m4_data_x)
m4_feat_df$type <-
  unlist(lapply(m4_data, function(ts)
    return(ts$type)))
m4_feat_df$period <-
  unlist(m4_period)

###########################################################################
# Forecast each TS using each of the benchmark methods ####

print("M4 Competition data:")
fcasts <-
  lapply(1:length(m4_data_x),
         multi_fit_ts,
         m4_data_x,
         m4_data_x_deseason,
         m4_data_x_horiz)
fcast_names <- names(fcasts[[1]])

###########################################################################
# Compute sMAPE, MASE, and OWA ####

fcast_errs <-
  lapply(1:length(fcasts),
         compute_fcast_errs,
         fcasts,
         m4_data_x,
         m4_data_xx)
mean_errs_df <-
  as.data.frame(lapply(fcast_names, mean_fcast_errs, fcast_errs))
colnames(mean_errs_df) <- fcast_names
mean_errs_df <-
  rbind(mean_errs_df,
        colMeans(mean_errs_df / mean_errs_df$naive2))
rownames(mean_errs_df) <- err_names
print(round(mean_errs_df, 3))

# fname <-
#   paste0(
#     "nts",
#     length(m4_data),
#     "_m4_",
#     tolower(substr(m4_season, 1, 3)),
#     "_tslen",
#     ts_len,
#     "_med_l2_nrep",
#     nrep
#   )
# title <-
#   paste0(
#     length(m4_data),
#     " TS from M4 ",
#     m4_season,
#     ", interpolated to length ",
#     ts_len,
#     ", Median + L2, clustering from k=",
#     min(k_range),
#     " to k=",
#     max(k_range),
#     ", ",
#     nrep,
#     " clustering reps:"
#   )
# print(title)
