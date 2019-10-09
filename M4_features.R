library(M4comp2018)
library(tsfeatures)
library(parallel)
library(GGally)
# library(ggplot2)
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
                     return(errs[1, ])))
colnames(fcast_smapes_df) <-
  paste0(colnames(fcast_smapes_df), "_smape")

fcast_mases_df <-
  bind_rows(lapply(fcast_errs,
                   function(errs)
                     return(errs[2, ])))
colnames(fcast_mases_df) <-
  paste0(colnames(fcast_mases_df), "_mase")

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

# khs %>%
#   select(Period, Entropy, Trend, Season, ACF1, Lambda) %>%
#   GGally::ggpairs()

###########################################################################
# Compute correlation matrix ####

m4_all_df <-
  bind_cols(
    khs %>%
      select(Period, Entropy, Trend, Season, ACF1, Lambda),
    fcast_smapes_df %>%
      select(
        naive2_smape,
        ses_smape,
        holt_smape,
        holt_damped_smape,
        theta_classic_smape,
        combined_smape
      ),
    fcast_mases_df %>%
      select(
        naive2_mase,
        ses_mase,
        holt_mase,
        holt_damped_mase,
        theta_classic_mase,
        combined_mase
      )
  )

# m4_all_df[is.na(m4_all_df)] <- 0.0
m4_all_df$Period <- as.numeric(as.character(m4_all_df$Period))
cor_base_mtx <- round(cor(m4_all_df), 2)
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

gg <- ggplot(cor_tri_df, aes(Var2, Var1, fill = value)) +
  ggtitle(
    paste0(
      "Feature cross correlation for different statistical methods. ",
      nrow(m4_all_df),
      " time series."
    )
  ) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name = "Correlation"
  ) +
  theme_minimal() + # minimal theme
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    size = 12,
    hjust = 1
  )) +
  theme(axis.text.y = element_text(
    vjust = 1,
    size = 12,
    hjust = 1
  )) +
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black",
            size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_colorbar(
    barwidth = 7,
    barheight = 1,
    title.position = "top",
    title.hjust = 0.5
  ))

print(gg)
if (!interactive()) {
  ggsave(
    "correlation_mtx.png",
    dpi = 100,
    scale = 5,
    width = 2,
    height = 2,
    units = "in"
  )
}

colnames(m4_all_df) <- sub("_", "\n", sub("_", "\n", colnames(m4_all_df)))
gg <- GGally::ggpairs(m4_all_df,
                      # upper = list(colour = "black"),
                      lower = list(
                        continuous = wrap("points", size = 0.2),
                        combo = wrap("dot", size = 0.2)
                      )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=0.1))
print(gg)

if (!interactive()) {
  ggsave(
    "ggallyn.png",
    dpi = 100,
    scale = 7,
    width = 2,
    height = 2,
    units = "in"
  )
}

# fit_lm <- function(method_err_name){
#   formula <- as.formula(paste(method_err_name, "~", paste(colnames(m4_feat_df)[1:(ncol(m4_feat_df)-2)], collapse='+')))
#   return(lm(formula, data = m4_all_df))
# }
# method_err_names <- c(colnames(fcast_smapes_df), colnames(fcast_mases_df))
# if (use_parallel) {
#   fits_lm <- mclapply(method_err_names, fit_lm, mc.cores = 16)
# } else {
#   fits_lm <- lapply(method_err_names, fit_lm)
# }
# names(fits_lm) <- method_err_names
# print(lapply(fits_lm, summary))

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
#   # colnames(cor_base_mtx) <- sub("_", " ", sub("_", " ", colnames(cor_base_mtx)))
#   # rownames(cor_base_mtx) <- sub("_", " ", sub("_", " ", rownames(cor_base_mtx)))
#   # write.csv(as.data.frame(cor_base_mtx), "correlation_mtx.csv")
#   # # cor_df <- as.data.frame(t(cor_base_mtx))
#   # # write.csv(cor_df[rev(rownames(cor_df)),], "correlation_mtx.csv")
#   ggsave(
#     "correlation_mtx.png",
#     dpi = 100,
#     scale = 5,
#     width = 2,
#     height = 2,
#     units = "in"
#   )
# }
#
# fit_lm <- function(method_err_name){
#   formula <- as.formula(paste(method_err_name, "~", paste(colnames(m4_feat_df)[1:(ncol(m4_feat_df)-2)], collapse='+')))
#   return(lm(formula, data = m4_all_df))
# }
# method_err_names <- c(colnames(fcast_smapes_df), colnames(fcast_mases_df))
# if (use_parallel) {
#   fits_lm <- mclapply(method_err_names, fit_lm, mc.cores = 16)
# } else {
#   fits_lm <- lapply(method_err_names, fit_lm)
# }
# names(fits_lm) <- method_err_names
# print(lapply(fits_lm, summary))
