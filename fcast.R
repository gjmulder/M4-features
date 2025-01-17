library(forecast)
source("benchmarks_eval.R")

multi_fit_ts <- function(idx, data_x, data_x_deseason, m4_data_x_horiz) {
  fcast_naive <- naive(data_x[[idx]], h = m4_data_x_horiz[[idx]])$mean
  fcast_seasonal_naive <-
    seasonal_naive(data_x[[idx]], fcast_horiz = m4_data_x_horiz[[idx]])
  # browser()
  fcast_naive2 <-
    naive(data_x_deseason[[idx]]$output, h = m4_data_x_horiz[[idx]])$mean * data_x_deseason[[idx]]$si_out
  fcast_ses <-
    ses(data_x_deseason[[idx]]$output, h = m4_data_x_horiz[[idx]])$mean * data_x_deseason[[idx]]$si_out
  fcast_holt <-
    tryCatch({
      holt(data_x_deseason[[idx]]$output,
           h = m4_data_x_horiz[[idx]],
           damped = FALSE)$mean * data_x_deseason[[idx]]$si_out
    }, error = function(err) {
      print(err)
      print(paste0(
        "..while generating Holt undamped for TS with index: ",
        idx,
        ". Using naive2 instead."
      ))
      return(fcast_naive2)
    })
  fcast_holt_damped <-
    tryCatch({
      holt(data_x_deseason[[idx]]$output,
           h = m4_data_x_horiz[[idx]],
           damped = TRUE)$mean * data_x_deseason[[idx]]$si_out
    }, error = function(err) {
      print(err)
      print(
        paste0(
          "..while generating Holt damped for TS with index: ",
          idx,
          ". Using naive2 instead."
        )
      )
      return(fcast_naive2)
    })
  fcast_theta_classic <-
    theta_classic(input = data_x_deseason[[idx]]$output, fcast_horiz = m4_data_x_horiz[[idx]])$mean * data_x_deseason[[idx]]$si_out

  return(
    list(
      naive          = fcast_naive,
      seasonal_naive = fcast_seasonal_naive,
      naive2         = fcast_naive2,
      ses            = fcast_ses,
      holt           = fcast_holt,
      holt_damped    = fcast_holt_damped,
      theta_classic  = fcast_theta_classic,
      combined       = (fcast_ses + fcast_holt + fcast_holt_damped) / 3
    )
  )
}

#######################################################################
# Compute sMAPE, MASE and OWA for all TS forecasts

compute_fcast_errs <- function(idx, fcasts_x, data_x, data_xx) {
  mean_smape <-
    lapply(fcasts_x[[idx]], function(fcast)
      return(mean(smape(fcast, data_xx[[idx]]))))
  mean_mase <-
    lapply(fcasts_x[[idx]], function(fcast)
      return(mean(mase(
        fcast, data_x[[idx]], data_xx[[idx]]
      ))))
  mean_errs <- bind_rows(mean_smape, mean_mase)
  return(mean_errs)
}

mean_fcast_errs <- function(fcast_name, fcast_errs) {
  smapes <- unlist(lapply(1:length(fcast_errs), function(idx)
    return(fcast_errs[[idx]][[fcast_name]][1])))
  mases <- unlist(lapply(1:length(fcast_errs), function(idx)
    return(fcast_errs[[idx]][[fcast_name]][2])))
  return(c(mean(smapes), mean(mases)))
}
