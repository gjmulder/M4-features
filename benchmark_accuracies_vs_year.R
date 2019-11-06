library(lubridate)
library(tidyverse)

imagenet <- read_csv("ImageNet_accuracies.csv")
wmt14 <- read_csv("WMT14_EnFr_accuracies.csv")
mnist <- read_csv("MNIST_accuracies2.csv")
m4_top_year <- read_csv("m4_accuracies.csv")

# ####################################################################
#
# imagenet %>%
#   filter(! is.na(`TOP 5 ACCURACY`)) %>%
#   group_by(YEAR) %>%
#   top_n(1, `TOP 5 ACCURACY`) %>%
#   filter(row_number(RANK) == 1) %>%
#   # mutate(accuracy = 100 * `TOP 5 ACCURACY`) %>%
#   mutate(normalised_accuracy = `TOP 5 ACCURACY`/min(.$`TOP 5 ACCURACY`)) %>%
#   select(normalised_accuracy, YEAR, METHOD) %>%
#   arrange(YEAR) ->
#   imagenet_top_year
# print(lm(formula = normalised_accuracy ~ YEAR, data = imagenet_top_year))
#
# imagenet_top_year %>%
#   ggplot(aes(x = YEAR, y = normalised_accuracy)) +
#   geom_point(size = 1) +
#   ggtitle("ImageNet Top-5 Accuracies") +
#   ylab("Normalised accuracy (2012 = 1.0)") +
#   xlab("Year model was published") +
#   # geom_text(
#   #   aes(y = normalised_accuracy, label = METHOD),
#   #   colour = "black", nudge_x = 1, nudge_y = 0.01, angle = -45
#   # ) +
#   # geom_hline(aes(yintercept = 95), colour = "red", linetype = "dashed") +
#   # geom_text(aes(y = 97, x = 2012.5), label = "Human level performance") +
#   geom_smooth(method = "lm", colour = "black", se = FALSE) ->
#   gg
#   # ylim(0, 100) -> gg
# print(gg)
#
# ####################################################################
#
# wmt14 %>%
#   filter(! is.na(`BLEU SCORE`)) %>%
#   filter(! is.na(YEAR)) %>%
#   filter(YEAR < 2019) %>%
#   group_by(YEAR) %>%
#   top_n(1, `BLEU SCORE`) %>%
#   mutate(normalised_bleu_score = `BLEU SCORE`/min(.$`BLEU SCORE`)) %>%
#   select(normalised_bleu_score, YEAR, METHOD) ->
#   wmt14_top_year
# print(lm(formula = normalised_bleu_score ~ YEAR, data = wmt14_top_year))
#
# wmt14_top_year %>%
#   ggplot(aes(x = YEAR, y = normalised_bleu_score)) +
#   geom_point(size = 1) +
#   ggtitle("WMT English to French Machine Translation") +
#   ylab("Normalised BLEU Score (2014 = 1.0)") +
#   xlab("Year model was published") +
#   # geom_text(
#   #   aes(y = 1.1, label = METHOD),
#   #   colour = "black", size = 5, angle = 90
#   # ) +
#   geom_smooth(method = "lm", colour = "black", se = FALSE) ->
#   gg
# print(gg)

####################################################################

# mnist %>%
#   mutate(year = as.integer(substr(Reference, nchar(Reference)-4, nchar(Reference)))) %>%
#   filter(! is.na(year)) %>%
#   filter(year < 2005 | year > 2009) %>%
#   filter(! is.na(`TEST ERROR RATE`)) %>%
#   group_by(year) %>%
#   top_n(-1, `TEST ERROR RATE`) %>%
#   filter(row_number(CLASSIFIER) == 1) %>%
#   mutate(normalised_test_error_rate = `TEST ERROR RATE`/max(.$`TEST ERROR RATE`)) %>%
#   select(normalised_test_error_rate, year, CLASSIFIER, DNN) %>%
#   arrange(year) ->
#   mnist_top_year
# print(lm(formula = normalised_test_error_rate ~ year, data = mnist_top_year))
#
# mnist_top_year %>%
#   ggplot(aes(x = year, y = normalised_test_error_rate)) +
#   geom_point(size = 0.5) +
#   ggtitle("MNIST Digit Recognition") +
#   ylab("Normalised test error percentage (1998 = 1.0)") +
#   xlab("Year model was published") +
#   # geom_text(
#   #   aes(y = 0.5, label = CLASSIFIER),
#   #   colour = "black", size = 5, angle = 90
#   # ) +
#   geom_smooth(method = "lm", colour = "black", se = FALSE) ->
#   gg
# print(gg)

mnist %>%
  arrange(desc(`TEST ERROR RATE`)) %>%
  mutate(rank = nrow(.) - row_number(`TEST ERROR RATE`)) ->
  mnist_rank

mnist_rank %>%
  ggplot() +
  geom_point(aes(x = rank, y = `TEST ERROR RATE`, colour = Type), size = 3) +
  geom_line(aes(x = rank, y = `TEST ERROR RATE`)) +
  # geom_text(aes(x = rank, y = `TEST ERROR RATE`, label = CLASSIFIER), angle = 45, nudge_x = 1, nudge_y = 0.1) ->
  gg
print(gg)

# ####################################################################
# m4_top_year %>%
#   filter(! is.na(YEAR)) %>%
#   mutate(normalised_smape = sMAPE / max(.$sMAPE)) ->
#   m4_top_year_norm
# print(lm(formula = normalised_smape ~ YEAR, data = m4_top_year_norm))
#
# m4_top_year_norm %>%
#   ggplot(aes(x = YEAR, y = normalised_smape)) +
#   geom_point(size = 1, fill = "blue") +
#   ggtitle("M4 Competition sMAPE") +
#   ylab("Normalised sMAPE (1957 = 1.0)") +
#   xlab("Year model was published") +
#   # geom_text(
#   #   aes(x = YEAR, y = normalised_smape, label = METHOD),
#   #   colour = "black", nudge_x = -1, nudge_y = -0.01
#   # ) +
#   geom_smooth(method = "lm", colour = "black", se = FALSE) -> gg
# print(gg)

m4_top_year %>%
  arrange(desc(sMAPE)) %>%
  mutate(rank = nrow(.) - row_number(sMAPE)) ->
  m4_rank

m4_rank %>%
  ggplot() +
  geom_point(aes(x = rank, y = sMAPE), size = 3) +
  geom_line(aes(x = rank, y = sMAPE)) +
  geom_text(aes(x = rank, y = sMAPE, label = METHOD), angle = 45, nudge_x = 0.1, nudge_y = 0.15) ->
  gg
print(gg)
