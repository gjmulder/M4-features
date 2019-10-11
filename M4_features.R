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

m4_st <-
  lapply(m4_data, function(ts)
    return(ts$st))

m4_type <-
  lapply(m4_data, function(ts)
    return(ts$type))

m4_period <-
  lapply(m4_data, function(ts)
    return(ts$period))

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

fcasts_slawek <- load_slawek_data(output_dir)

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

# ###########################################################################
# # Compute sMAPE and MASE ####
#
# if (use_parallel) {
#   fcast_errs <- mclapply(1:length(fcasts),
#                          compute_fcast_errs,
#                          fcasts,
#                          m4_data_x,
#                          m4_data_xx,
#                          mc.cores = 16)
# } else {
#   fcast_errs <- lapply(1:length(fcasts),
#                        compute_fcast_errs,
#                        fcasts,
#                        m4_data_x,
#                        m4_data_xx)
# }
#
# fcast_smapes_df <-
#   unlist(lapply(fcast_errs,
#                 function(errs)
#                   return(names(which.min(
#                     errs[1,]
#                   )))))
#
# fcast_mases_df <-
#   unlist(lapply(fcast_errs,
#                 function(errs)
#                   return(names(which.min(
#                     errs[2,]
#                   )))))
#
# m4_data_all_df <-
#   tibble(
#     best_mases = fcast_mases_df,
#     best_smapes = fcast_smapes_df,
#     type = as.character(unlist(m4_type)),
#     period = as.character(unlist(m4_period))
#   )
#
# prop_str <- function(x) {
#   pt <- round(100 * prop.table(table(x)), 1)
#   return(paste0(names(pt), sprintf(":%5.1f%%", pt), "\n", collapse = ''))
# }
#
# ###########################################################################
# # Generate percentage best method table for each period and type ####
#
# m4_data_all_df %>%
#   group_by(type, period) %>%
#   summarise(data = prop_str(best_mases)) ->
#   m4_type_period_df
#
# m4_data_all_df %>%
#   group_by(type) %>%
#   summarise(data = prop_str(best_mases)) %>%
#   mutate(period = "Total") ->
#   m4_type_df
#
# m4_data_all_df %>%
#   summarise(data = prop_str(best_mases)) %>%
#   mutate(period = "Total") %>%
#   mutate(type = "Total") ->
#   m4_total_df
#
# m4_data_all_df %>%
#   group_by(period) %>%
#   summarise(data = prop_str(best_mases)) %>%
#   mutate(type = "Total") ->
#   m4_period_df
#
# bind_rows(m4_type_period_df, m4_type_df, m4_period_df, m4_total_df) %>%
#   spread(type, data) %>%
#   select(Micro, Industry, Macro, Finance, Demographic, Other, Total, period) ->
#   results_df
#
# results_df <- as.data.frame(results_df)
# rownames(results_df) <- results_df$period
# results_df$period <- NULL
#
# tt <- gridExtra::ttheme_default(
#   core = list(fg_params=list(cex = 0.8)),
#   colhead = list(fg_params=list(cex = 0.8)),
#   rowhead = list(fg_params=list(cex = 0.8)))
# dev.off()
# print(grid.table(results_df[c(7, 4, 3, 6, 1, 2, 5),], theme=tt))
