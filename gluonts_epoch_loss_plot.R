# awk 'BEGIN {print "Date, Epoch.Loss"} /Evaluation metric/ {print $1 " " $2 ", " substr($NF, 14)}' m4_daily_DeepAREstimator.log > ../../Work/M4-features/m4_daily_epoch_loss.csv

gluonts <- read_csv("m4_daily_epoch_loss.csv")

cumulative_best_loss <- rep(10.0, 1, nrow(gluonts))
cumulative_best_loss_epoch <- rep(NA, 1, nrow(gluonts))
for (idx in 1:nrow(gluonts)) {
  if (gluonts$Epoch.Loss[idx] < min(cumulative_best_loss)) {
    cumulative_best_loss[idx] <- gluonts$Epoch.Loss[idx]
    cumulative_best_loss_epoch[idx] <- idx * 10
  } else {
    cumulative_best_loss[idx] <- min(cumulative_best_loss)
  }
}

gluonts$cumulative.loss <-
  cumulative_best_loss
gluonts$cumulative.loss.epoch <-
  cumulative_best_loss_epoch

gg <-
  ggplot(gluonts) +
  geom_point(aes(x = Date, y = Epoch.Loss), size = 1) +
  geom_smooth(aes(x = Date, y = Epoch.Loss)) +
  geom_line(aes(x = Date, y = cumulative.loss), linetype = "dashed", colour = "red") +
  geom_text(aes(x = Date, y = cumulative.loss, label = cumulative.loss.epoch), angle = 45, nudge_x = -3000, nudge_y = -0.015) +
  ylim(5.0, 5.4) +
  ggtitle("GluonTS Training Loss Optimisation DeepAR M4 Hourly")
print(gg)
