library(M4comp2018)
library(tsfeatures)
library(parallel)
library(gplots)
library(ggplot2)
library(tidyverse)

set.seed(42)
options(warn = 2, width = 1024)
source("fcast.R")

###########################################################################
# Config ####

if (interactive()) {
  prop_ts <- 0.01
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

fcast_smapes_df <-
  bind_rows(lapply(fcast_errs,
                   function(errs)
                     return(errs[1,])))
colnames(fcast_smapes_df) <-
  paste0(colnames(fcast_smapes_df), "_smape")

fcast_mases_df <-
  bind_rows(lapply(fcast_errs,
                   function(errs)
                     return(errs[2,])))
colnames(fcast_mases_df) <-
  paste0(colnames(fcast_mases_df), "_mase")

###########################################################################
# Features ####

m4_feat_df <- tsfeatures(m4_data_x, parallel = use_parallel)
m4_feat_df$type <-
  unlist(lapply(m4_data, function(ts)
    return(ts$type)))
m4_feat_df$period <-
  unlist(m4_period)

###########################################################################
# Combine everything ####

m4_all_df <-
  bind_cols(fcast_smapes_df, fcast_mases_df, m4_feat_df)

# m4_all_df %>%
#   group_by(period, type) %>%
#   summarise_all(mean, na.rm = TRUE) ->
#   m4_sum_df
#
# mtx <- as.matrix(m4_sum_df[3:ncol(m4_sum_df)])
# rownames(mtx) <-
#   map_chr(1:nrow(m4_sum_df), function(r, df)
#     return(sprintf("%s, %s", df[r,]$period, df[r,]$type)), m4_sum_df)
#
# png("heatmap.png", width = 2048, height = 2048)
# heatmap.2(
#   mtx,
#   srtCol = 45,
#   cexCol = 2,
#   cexRow = 2,
#   scale = "column",
#   margins = c(16, 24),
#   main = paste0("M4 Mean Features per Frequency and Domain for ", length(m4_data_x), " M4 time series")
# )
# dev.off()

###########################################################################
# Compute correlation matrix ####

feature_nas <- colSums(is.na(m4_all_df))
print("Features with NA values:")
print(feature_nas[feature_nas > 0])
m4_all_df[is.na(m4_all_df)] <- 0.0
cor_base_mtx <- round(cor(m4_all_df[1:(ncol(m4_all_df) - 2)]), 2)
cor_base_mtx[lower.tri(cor_base_mtx)] <- NA

cor_tri_df <- as.data.frame(cor_base_mtx) %>%
  mutate(Var1 = factor(row.names(.), levels = row.names(.))) %>%
  gather(
    key = Var2,
    value = value,
    -Var1,
    na.rm = TRUE,
    factor_key = TRUE
  )

gg <-
  ggplot(data = cor_tri_df, aes(Var1, Var2, fill = value)) +
  ggtitle(
    paste0(
      "Correlation Matrix for time series features, sMAPEs and MASEs for ",
      length(m4_data_x),
      " M4 time series"
    )
  ) +
  geom_tile() +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))
print(gg)

if (!interactive()) {
  cor_df <- as.data.frame(t(cor_base_mtx))
  write.csv(cor_df[rev(rownames(cor_df)),], "correlation_mtx.csv")
  ggsave(
    "correlation_mtx.png",
    dpi = 100,
    scale = 5,
    width = 2,
    height = 2,
    units = "in"
  )
}
