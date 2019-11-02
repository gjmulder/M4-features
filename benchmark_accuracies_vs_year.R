library(lubridate)
library(tidyverse)

imagenet <- read_csv("ImageNet_accuracies.csv")
wmt14 <- read_csv("WMT14_EnFr_accuracies.csv")
mnist <- read_csv("MNIST_accuracies.csv")
m4_top_year <- read_csv("m4_accuracies.csv")

imagenet %>%
  filter(! is.na(`TOP 5 ACCURACY`)) %>%
  group_by(YEAR) %>%
  top_n(1, `TOP 5 ACCURACY`) %>%
  filter(row_number(RANK) == 1) %>%
  mutate(Accuracy = 100 * `TOP 5 ACCURACY`) %>%
  select(Accuracy, YEAR, METHOD) %>%
  arrange(YEAR) ->
  imagenet_top_year

tibble(
  year = deframe(imagenet_top_year[2:nrow(imagenet_top_year), "YEAR"]),
  method = deframe(imagenet_top_year[2:nrow(imagenet_top_year), "METHOD"]),
  accuracy.score.last = deframe(imagenet_top_year[1:(nrow(imagenet_top_year) - 1), "Accuracy"]),
  year.diff = diff(imagenet_top_year$YEAR),
  accuracy.score.diff = diff(imagenet_top_year$Accuracy)
) %>%
  mutate(yoy.percentage = 100 * accuracy.score.diff / year.diff  / accuracy.score.last) ->
  imagenet_top_year_diff

imagenet_top_year_diff %>%
  ggplot(aes(x = year, y = yoy.percentage)) +
  geom_col(width = 0.5, fill = "blue") +
  ggtitle("ImageNet Top-5 Accuracies") +
  ylab("Your-on-year % improvement") +
  xlab("Year model was published") +
  geom_text(
    aes(y = 4, label = method),
    colour = "black", size = 5, angle = 90
  ) +
  # geom_hline(aes(yintercept = 95), colour = "red", linetype = "dashed") +
  # geom_text(aes(y = 97, x = 2012.5), label = "Human level performance") +
  geom_smooth(method = "lm", colour = "black", se = FALSE) -> gg
print(gg)

wmt14 %>%
  filter(! is.na(`BLEU SCORE`)) %>%
  filter(! is.na(YEAR)) %>%
  filter(YEAR < 2019) %>%
  group_by(YEAR) %>%
  top_n(1, `BLEU SCORE`) %>%
  select(`BLEU SCORE`, YEAR, METHOD) %>%
  arrange(YEAR) ->
  wmt14_top_year

tibble(
  year = deframe(wmt14_top_year[2:nrow(wmt14_top_year), "YEAR"]),
  method = deframe(wmt14_top_year[2:nrow(wmt14_top_year), "METHOD"]),
  bleu.score.last = deframe(wmt14_top_year[1:(nrow(wmt14_top_year) - 1), "BLEU SCORE"]),
  year.diff = diff(wmt14_top_year$YEAR),
  bleu.score.diff = diff(wmt14_top_year$`BLEU SCORE`)
) %>%
  mutate(yoy.percentage = 100 * bleu.score.diff / year.diff  / bleu.score.last) ->
  wmt14_top_year_diff

wmt14_top_year_diff %>%
  ggplot(aes(x = year, y = yoy.percentage)) +
  geom_col(width = 0.5, fill = "blue") +
  ggtitle("WMT English to French Machine Translation") +
  ylab("Year-on-year % improvement") +
  xlab("Year model was published") +
  geom_text(
    aes(y = 2, label = method),
    colour = "black", size = 5, angle = 90
  ) +
  geom_smooth(method = "lm", colour = "black", se = FALSE) ->
  gg

print(gg)

# mnist %>%
#   filter(! is.na(YEAR)) %>%
#   arrange(desc(`PERCENTAGE ERROR`)) %>%
#   group_by(YEAR) %>%
#   top_n(-1, `PERCENTAGE ERROR`) %>%
#   filter(row_number(RANK) == 1) %>%
#   select(`PERCENTAGE ERROR`, YEAR, METHOD) %>%
#   arrange(YEAR) ->
#   mnist_top_year
#
# mnist_top_year %>%
#   ggplot(aes(x = YEAR, y = `PERCENTAGE ERROR`)) +
#   geom_col(width = 0.5, fill = "blue") +
#   ggtitle("MNIST Digit Recognition") +
#   ylab("% accuracy") +
#   xlab("Year model was published") +
#   geom_text(
#     aes(y = 0.15, label = METHOD),
#     colour = "black", size = 5, angle = 90
#   ) +
#   geom_smooth(method = "lm", colour = "black", se = FALSE) ->
#   gg
#
# print(gg)

m4_top_year %>%
  filter(! is.na(YEAR)) %>%
  ggplot(aes(x = YEAR, y = sMAPE)) +
  geom_col(width = 2, fill = "blue") +
  ggtitle("M4 Competition sMAPE") +
  ylab("sMAPE") +
  xlab("Year model was published") +
  geom_text(
    aes(y = 7, label = METHOD),
    colour = "black", size = 5, angle = 90
  ) +
  geom_smooth(method = "lm", colour = "black", se = FALSE) -> gg

print(gg)
