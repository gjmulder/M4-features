library(tidyverse)

# cd ~jenkins/jobs/;awk 'BEGIN {print "Trial MASE";rank=1} /MASE/ {print rank, $12;rank+=1}' ./gpu_*/configurations/axis-INSTANCE/*/axis-label/*/builds/*/log | tr " ," ", "

hyperopt <- read_csv("hyperopt_m4_hourly.csv")

cumulative_best_mase <- c(10)
for (MASE in hyperopt$MASE) {
    best_mase <- min(MASE, cumulative_best_mase)
    cumulative_best_mase <- c(cumulative_best_mase, best_mase)
}

hyperopt$cumulative.mase <-
  cumulative_best_mase[2:length(cumulative_best_mase)]

gg <-
  ggplot(hyperopt) +
  geom_point(aes(x = Trial, y = MASE)) +
  geom_smooth(aes(x = Trial, y = MASE)) +
  geom_line(aes(x = Trial, y = cumulative.mase), linetype = "dashed", colour = "red") +
  ggtitle("HyperOpt MASE optimisation")
print(gg)
