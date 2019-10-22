# library(M4comp2018)
library(Mcomp)
library(tsfeatures)
library(parallel)
# library(GGally)
library(grid)
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
  num_cores <- 6
} else
{
  prop_ts <- NA
  num_cores <- 16
}
use_parallel <- TRUE #is.na(prop_ts)
m_freqs <- read_csv("m4_horiz.csv")
horizons <- as.list(m_freqs$Horizon)
names(horizons) <- m_freqs$SP
err_names <- c("sMAPE", "MASE", "OWA")
slawek_output_dir <-
  "/home/mulderg/Work/118 - slaweks17/github/c++/output/"

###########################################################################
# Preprocess M4 data ####

# M4 <- Filter(function(ts)
#   ts$period == "Hourly", M4)

if (is.na(prop_ts)) {
  m_data <- M3
} else {
  m_data <- sample(M4, prop_ts * length(M3))
}

m_data_x <-
  lapply(m_data, function(ts)
    return(ts$x))

m_st <-
  lapply(m_data, function(ts)
    return(ts$st))

m_type <-
  lapply(m_data, function(ts)
    return(ts$type))

m_period <-
  lapply(m_data, function(ts)
    return(ts$period))

m_horiz <-
  lapply(1:length(m_data_x), function(idx)
    return(m_data[[idx]]$h))

if (use_parallel) {
  m_data_x_deseason <- mclapply(1:length(m_data_x), function(idx)
    return(deseasonalise(m_data_x[[idx]], m_horiz[[idx]])), mc.cores = num_cores)
} else {
  m_data_x_deseason <- lapply(1:length(m_data_x), function(idx)
    return(deseasonalise(m_data_x[[idx]], m_horiz[[idx]])))
}

m_data_xx <-
  lapply(m_data, function(ts)
    return(ts$xx))

###########################################################################
# Forecast each TS using each of the benchmark methods ####

print("M-Competition data:")

if (use_parallel) {
  fcasts <- mclapply(
    1:length(m_data_x),
    multi_fit_ts,
    m_data_x,
    m_data_x_deseason,
    m_horiz,
    mc.cores = num_cores
  )
} else {
  fcasts <- lapply(1:length(m_data_x),
                   multi_fit_ts,
                   m_data_x,
                   m_data_x_deseason,
                   m_horiz)
}

fcasts_all <- fcasts
# fcasts_slawek <- load_slawek_data(slawek_output_dir)
# if (use_parallel) {
#   fcasts_all <- mclapply(1:length(m_data_x),
#                          function(idx)
#                            return(c(fcasts[[idx]], list(slawek = fcasts_slawek[[m_st[[idx]]]]))),
#                          mc.cores = num_cores)
# } else {
#   fcasts_all <- lapply(1:length(m_data_x),
#                        function(idx)
#                          return(c(fcasts[[idx]], list(slawek = fcasts_slawek[[m_st[[idx]]]]))))
# }
fcast_names <- names(fcasts_all[[1]])

###########################################################################
# Compute sMAPE and MASE ####

print("Computing error metrics:")
if (use_parallel) {
  fcast_errs <- mclapply(
    1:length(fcasts),
    compute_fcast_errs,
    fcasts_all,
    m_data_x,
    m_data_xx,
    mc.cores = num_cores
  )
} else {
  fcast_errs <- lapply(1:length(fcasts),
                       compute_fcast_errs,
                       fcasts_all,
                       m_data_x,
                       m_data_xx)
}

mean_errs_df <-
  as.data.frame(lapply(fcast_names, mean_fcast_errs, fcast_errs))
colnames(mean_errs_df) <- fcast_names
mean_errs_df <-
  rbind(mean_errs_df,
        colMeans(mean_errs_df / mean_errs_df$naive2))
rownames(mean_errs_df) <- err_names

# fcasts_idx <- c(1:length(fcasts_all))[(unlist(m_period) == "YEARLY") & (unlist(m_type) == "OTHER")]
# bind_rows(lapply(fcasts_idx, function(idx) return(fcast_errs[[idx]][1,])))

###########################################################################
# Generate percentage best method table for each period and type ####

fcast_smapes_df <-
  unlist(lapply(fcast_errs,
                function(errs)
                  return(names(which.min(
                    errs[1,]
                  )))))

fcast_mases_df <-
  unlist(lapply(fcast_errs,
                function(errs)
                  return(names(which.min(
                    errs[2,]
                  )))))

m_data_all_df <-
  tibble(
    best_mases = fcast_mases_df,
    best_smapes = fcast_smapes_df,
    type = as.character(unlist(m_type)),
    period = as.character(unlist(m_period))
  )

prop_str <- function(x) {
  pt <- round(100 * prop.table(table(x)), 1)
  return(paste0(names(pt), sprintf(":%5.1f%%", pt), "\n", collapse = ''))
}

m_data_all_df %>%
  group_by(type, period) %>%
  summarise(data = prop_str(best_smapes)) ->
  m_type_period_df

m_data_all_df %>%
  group_by(type) %>%
  summarise(data = prop_str(best_smapes)) %>%
  mutate(period = "Total") ->
  m_type_df

m_data_all_df %>%
  summarise(data = prop_str(best_smapes)) %>%
  mutate(period = "Total") %>%
  mutate(type = "Total") ->
  m_total_df

m_data_all_df %>%
  group_by(period) %>%
  summarise(data = prop_str(best_smapes)) %>%
  mutate(type = "Total") ->
  m_period_df

bind_rows(m_type_period_df, m_type_df, m_period_df, m_total_df) %>%
  spread(type, data) %>%
  select(MICRO,
         INDUSTRY,
         MACRO,
         FINANCE,
         DEMOGRAPHIC,
         OTHER,
         Total,
         period) ->
  results_df

results_df <- as.data.frame(results_df)
rownames(results_df) <- results_df$period
results_df$period <- NULL

###########################################################################
# Report data and plots ####

print(round(mean_errs_df, 3))

if (!is.null(dev.list()))
  grid.newpage()

tt <- ttheme_default(
  core = list(fg_params = list(cex = 0.8)),
  colhead = list(fg_params = list(cex = 0.8)),
  rowhead = list(fg_params = list(cex = 0.8))
)
print(grid.table(results_df[c(5, 3, 1, 2, 4),]))

# gg_holt <-
#   ggplot(tibble(vs_holt = vs_holt)) +
#   geom_histogram(aes(x = vs_holt), bins = 100) +
#   scale_y_sqrt() +
#   ggtitle("Histogram of (Holt Classic MAPE - Slawek MAPE) for Monthly and Quarterly (72K TS)")
# # print(gg_holt)

# gg_theta <-
#   ggplot(tibble(vs_theta = vs_theta)) +
#   geom_histogram(aes(x = vs_theta), bins = 100) +
#   scale_y_sqrt() +
#   ggtitle("Histogram of (Theta CLassic - Slawek MAPE) for Monthly and Quarterly (72K TS)")
# print(gg_theta)

###########################################################################
# Batch write to ./results ####

if (!interactive()) {
  write_csv(mean_errs_df, "results/mean_errors.csv")
  png(filename = "results/fcast_percentages.png",
      width = 2048,
      height = 2048)
  print(grid.table(results_df))
  dev.off()

  # write.csv(
  #   as.data.frame(table(round(vs_holt))),
  #   "results/holt_mape_sub_slawek_mape_freqs.csv",
  #   row.names = FALSE
  # )
  # ggsave("holt_mape_sub_slawek_mape_histo.png", gg_holt)
  #
  # write.csv(
  #   as.data.frame(table(round(vs_theta))),
  #   "results/theta_mape_sub_slawek_mape_freqs.csv",
  #   row.names = FALSE
  # )
  # ggsave("theta_mape_sub_slawek_mape_histo.png", gg_theta)
}
