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

m4_data_all_df <-
  tibble(
    best_mases = fcast_mases_df,
    best_smapes = fcast_smapes_df,
    type = as.character(unlist(m4_type)),
    period = as.character(unlist(m4_period))
  )

prop_str <- function(x) {
  pt <- round(100 * prop.table(table(x)))
  return(paste0(names(pt), sprintf(":%4d%%", pt), "\n", collapse = ''))
}

m4_data_all_df %>%
  group_by(type, period) %>%
  summarise(data = prop_str(best_mases)) ->
  m4_type_period_df

m4_data_all_df %>%
  group_by(type) %>%
  summarise(data = prop_str(best_mases)) %>%
  mutate(period = "Total") ->
  m4_type_df

m4_data_all_df %>%
  summarise(data = prop_str(best_mases)) %>%
  mutate(period = "Total") %>%
  mutate(type = "Total") ->
  m4_total_df

m4_data_all_df %>%
  group_by(period) %>%
  summarise(data = prop_str(best_mases)) %>%
  mutate(type = "Total") ->
  m4_period_df

bind_rows(m4_type_period_df, m4_type_df, m4_period_df, m4_total_df) %>%
  spread(type, data) %>%
  arrange(c(7, 4, 3, 6, 1, 2, 5)) ->
  results_df

dev.off()
print(grid.table(results_df))

# ,
# rows = rep(
#   "Entropy\nTrend\nSeason\nACF1\nLambda\nMASE\nsMAPE",
#   nrow(results_df)
# ),
# cols = col_names
# # print(grid.table(results_df, rows = rep("Entropy\nMASE", nrow(results_df)), cols = col_names))
# select(Micro, Industry, Macro, Finance, Demographic, Other, Total, period) %>%
# arrange(c(7, 1, 2, 3, 4, 5, 6)) ->
#
# col_names <-  paste0(colnames(results_df), "\nmean   sd")
# col_names[length(col_names)] <- "Freq"
#
# # png('feat_freqs.png', width = 4096, height = 4096)

# ###########################################################################
# # Compute correlation matrix ####
#
# m4_all_df <-
#   bind_cols(
#     khs %>%
#       select(Period, Entropy, Trend, Season, ACF1, Lambda),
#     fcast_smapes_df %>%
#       select(
#         naive2_smape,
#         ses_smape,
#         holt_smape,
#         holt_damped_smape,
#         theta_classic_smape,
#         combined_smape
#       ),
#     fcast_mases_df %>%
#       select(
#         naive2_mase,
#         ses_mase,
#         holt_mase,
#         holt_damped_mase,
#         theta_classic_mase,
#         combined_mase
#       )
#   )
#
# # m4_all_df[is.na(m4_all_df)] <- 0.0
# m4_all_df$period <- as.numeric(m4_all_df$period)
# m4_all_df$type <- as.numeric(m4_all_df$type)
# cor_base_mtx <- round(cor(m4_all_df), 2)
# cor_base_mtx[lower.tri(cor_base_mtx)] <- NA
#
# cor_tri_df <- as.data.frame(cor_base_mtx) %>%
#   mutate(Var1 = factor(row.names(.), levels = row.names(.))) %>%
#   gather(
#     key = Var2,
#     value = value,
#     -Var1,
#     na.rm = TRUE,
#     factor_key = TRUE
#   )
#
# gg <- ggplot(cor_tri_df, aes(Var2, Var1, fill = value)) +
#   ggtitle(
#     paste0(
#       "Feature cross correlation for different statistical methods. ",
#       nrow(m4_all_df),
#       " time series."
#     )
#   ) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(
#     low = "blue",
#     high = "red",
#     mid = "white",
#     midpoint = 0,
#     limit = c(-1, 1),
#     space = "Lab",
#     name = "Correlation"
#   ) +
#   theme_minimal() + # minimal theme
#   theme(axis.text.x = element_text(
#     angle = 45,
#     vjust = 1,
#     size = 12,
#     hjust = 1
#   )) +
#   theme(axis.text.y = element_text(
#     vjust = 1,
#     size = 12,
#     hjust = 1
#   )) +
#   coord_fixed() +
#   geom_text(aes(Var2, Var1, label = value),
#             color = "black",
#             size = 4) +
#   theme(
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.border = element_blank(),
#     panel.background = element_blank(),
#     axis.ticks = element_blank(),
#     legend.justification = c(1, 0),
#     legend.position = c(0.6, 0.7),
#     legend.direction = "horizontal"
#   ) +
#   guides(fill = guide_colorbar(
#     barwidth = 7,
#     barheight = 1,
#     title.position = "top",
#     title.hjust = 0.5
#   ))
#
# print(gg)
# if (!interactive()) {
#   ggsave(
#     "correlation_mtx.png",
#     dpi = 100,
#     scale = 5,
#     width = 2,
#     height = 2,
#     units = "in"
#   )
# }
