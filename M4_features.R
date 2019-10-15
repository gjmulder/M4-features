library(M4comp2018)
library(tsfeatures)
library(parallel)
# library(GGally)
library(gridExtra)
# library(ggplot2)
library(tidyverse)

set.seed(42)
options(warn = 2)
options(width = 1024)
source("fcast.R")

###########################################################################
# Config ####

if (interactive()) {
  prop_ts <- NA
  num_cores <- 2
} else
{
  prop_ts <- NA
  num_cores <- 16
}
use_parallel <- TRUE #is.na(prop_ts)
m4_freqs <- read_csv("m4_horiz.csv")
horizons <- as.list(m4_freqs$Horizon)
names(horizons) <- m4_freqs$SP
err_names <- c("sMAPE", "MASE", "OWA")
slawek_output_dir <-
  "/home/mulderg/Work/118 - slaweks17/github/c++/output/"

###########################################################################
# Preprocess M4 data ####

M4 <- Filter(function(ts)
  ts$period == "Quarterly" | ts$period == "Monthly", M4)

if (is.na(prop_ts)) {
  m4_data <- M4
} else {
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
    return(deseasonalise(m4_data_x[[idx]], m4_horiz[[idx]])), mc.cores = num_cores)
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
    mc.cores = num_cores
  )
} else {
  fcasts <- lapply(1:length(m4_data_x),
                   multi_fit_ts,
                   m4_data_x,
                   m4_data_x_deseason,
                   m4_horiz)
}

# fcasts_all <- fcasts
fcasts_slawek <- load_slawek_data(slawek_output_dir)
if (use_parallel) {
  fcasts_all <- mclapply(1:length(m4_data_x),
                         function(idx)
                           return(c(fcasts[[idx]], list(slawek = fcasts_slawek[[m4_st[[idx]]]]))),
                         mc.cores = num_cores)
} else {
  fcasts_all <- lapply(1:length(m4_data_x),
                       function(idx)
                         return(c(fcasts[[idx]], list(slawek = fcasts_slawek[[m4_st[[idx]]]]))))
}
fcast_names <- names(fcasts_all[[1]])

###########################################################################
# Compute sMAPE and MASE ####

print("Computing error metrics:")
if (use_parallel) {
  fcast_errs <- mclapply(
    1:length(fcasts),
    compute_fcast_errs,
    fcasts_all,
    m4_data_x,
    m4_data_xx,
    mc.cores = num_cores
  )
} else {
  fcast_errs <- lapply(1:length(fcasts),
                       compute_fcast_errs,
                       fcasts_all,
                       m4_data_x,
                       m4_data_xx)
}

mean_errs_df <-
  as.data.frame(lapply(fcast_names, mean_fcast_errs, fcast_errs))
colnames(mean_errs_df) <- fcast_names
mean_errs_df <-
  rbind(mean_errs_df,
        colMeans(mean_errs_df / mean_errs_df$naive2))
rownames(mean_errs_df) <- err_names

vs_holt <-
  unlist(lapply(fcast_errs,
                function(errs)
                  return(errs[1, "holt"] - errs[1, "slawek"])))

vs_theta <-
  unlist(lapply(fcast_errs,
                function(errs)
                  return(errs[1, "theta_classic"] - errs[1, "slawek"])))

m4_data_all_df <-
  tibble(
    vs_holt = vs_holt,
    vs_theta = vs_theta,
    type = as.character(unlist(m4_type)),
    period = as.character(unlist(m4_period))
  )

build_str <- function(vs_holt, vs_theta) {
  names <-
    c("vs_holt_mean",
      "vs_holt_sd",
      "vs_theta_mean",
      "vs_theta_sd")
  percs <-
    c(round(mean(vs_holt) * 100, 1),
      round(sd(vs_holt) * 100, 1),
      round(mean(vs_theta) * 100, 1),
      round(sd(vs_theta) * 100, 1))
  return(paste0(names, sprintf(":%5.1f%%", percs), "\n", collapse = ''))
}

###########################################################################
# Generate percentage best method table for each period and type ####

m4_data_all_df %>%
  group_by(type, period) %>%
  summarise(data = build_str(vs_holt, vs_theta)) ->
  m4_type_period_df

m4_data_all_df %>%
  group_by(type) %>%
  summarise(data = build_str(vs_holt, vs_theta)) %>%
  mutate(period = "Total") ->
  m4_type_df

m4_data_all_df %>%
  group_by(period) %>%
  summarise(data = build_str(vs_holt, vs_theta)) %>%
  mutate(type = "Total") ->
  m4_period_df

m4_data_all_df %>%
  summarise(data = build_str(vs_holt, vs_theta)) %>%
  mutate(period = "Total") %>%
  mutate(type = "Total") ->
  m4_total_df

bind_rows(m4_type_period_df, m4_type_df, m4_period_df, m4_total_df) %>%
  spread(type, data) %>%
  select(Micro,
         Industry,
         Macro,
         Finance,
         Demographic,
         Other,
         Total,
         period) ->
  results_df

results_df <- as.data.frame(results_df)
rownames(results_df) <- results_df$period
results_df$period <- NULL

###########################################################################
# Report data and plots ####

if (!is.null(dev.list()))
  dev.off()
print(grid.table(results_df[c(7, 4, 3, 6, 1, 2, 5), ], theme = tt))

gg_holt <-
  ggplot(tibble(vs_holt = vs_holt)) +
  geom_histogram(aes(x = vs_holt), bins = 100) +
  scale_y_sqrt() +
  ggtitle("Histogram of (Holt Classic MAPE - Slawek MAPE) for Monthly and Quarterly (72K TS)")
# print(gg_holt)

gg_theta <-
  ggplot(tibble(vs_theta = vs_theta)) +
  geom_histogram(aes(x = vs_theta), bins = 100) +
  scale_y_sqrt() +
  ggtitle("Histogram of (Theta CLassic - Slawek MAPE) for Monthly and Quarterly (72K TS)")
# print(gg_theta)

print(round(mean_errs_df, 3))

###########################################################################
# Batch write to ./results ####

if (!interactive()) {
  write_csv(mean_errs_df, "results/mean_errors.csv")
  png(filename = "results/fcast_percentages.png",
      width = 2048,
      height = 2048)
  tt <- gridExtra::ttheme_default(
    core = list(fg_params = list(cex = 0.8)),
    colhead = list(fg_params = list(cex = 0.8)),
    rowhead = list(fg_params = list(cex = 0.8))
  )  print(grid.table(results_df[c(7, 4, 3, 6, 1, 2, 5), ], theme = tt))
  dev.off()

  write.csv(as.data.frame(table(round(vs_holt))), "results/holt_mape_sub_slawek_mape_freqs.csv", row.names = FALSE)
  ggsave("holt_mape_sub_slawek_mape_histo.png", gg_holt)

  write.csv(as.data.frame(table(round(vs_theta))), "results/theta_mape_sub_slawek_mape_freqs.csv", row.names = FALSE)
  ggsave("theta_mape_sub_slawek_mape_histo.png", gg_theta)
}
