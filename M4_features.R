library(M4comp2018)
# library(tsfeatures)
library(parallel)
library(tidyverse)

set.seed(42)
options(warn = 0, width = 1024)
source("fcast.R")

###########################################################################
# Config ####

if (interactive()) {
  prop_ts <- NA
} else
{
  prop_ts <- NA
}
use_parallel <- TRUE #is.na(prop_ts)
err_names <- c("sMAPE", "MASE", "OWA")

###########################################################################
# Preprocess M4 data ####

if (is.na(prop_ts)) {
  m4_data <- M4
} else {
  m4_data <- Filter(function(ts)
    ts$period == "Monthly", M4)
  # m4_data <- sample(M4, prop_ts * length(M4))
}

m4_data_x <-
  lapply(m4_data, function(ts)
    return(ts$x))

# m4_type <-
#   lapply(m4_data, function(ts)
#     return(ts$type))
# names(m4_type) <- "type"
#
# m4_period <-
#   lapply(m4_data, function(ts)
#     return(ts$period))
# names(m4_period) <- "period"

m4_horiz <-
  lapply(1:length(m4_data_x), function(idx)
    return(m4_data[[idx]]$h))

if (use_parallel) {
  m4_data_x_deseason <- mclapply(1:length(m4_data_x), function(idx)
    return(deseasonalise(m4_data_x[[idx]], m4_horiz[[idx]])), mc.cores = 2)
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
    mc.cores = 2
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
                         mc.cores = 2)
} else {
  fcast_errs <- lapply(1:length(fcasts),
                       compute_fcast_errs,
                       fcasts,
                       m4_data_x,
                       m4_data_xx)
}

###########################################################################
# Compute error summaries ####

mean_errs_df <-
  as.data.frame(lapply(fcast_names, mean_fcast_errs, fcast_errs))
colnames(mean_errs_df) <- fcast_names
mean_errs_df <-
  rbind(mean_errs_df,
        colMeans(mean_errs_df / mean_errs_df$naive2))
rownames(mean_errs_df) <- err_names
print(round(mean_errs_df, 3))
# write_csv(as.data.frame(t(mean_errs_df)), path = paste0("benchmark_m4_", m4_season, ".csv"))

algo_dates <-
  c(NA, NA, NA, 1957, 1960, 1985, 2000, 1993, 1962)
m4_res <-
  tibble(YEAR = algo_dates, METHOD = names(mean_errs_df), sMAPE=as.vector(t(mean_errs_df[1,])))
colnames(m4_res) <- c("YEAR", "METHOD", "sMAPE")
write_csv(m4_res, "m4_accuracies.csv")
