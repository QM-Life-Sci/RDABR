library(tidyverse)
library(readxl)
library(DescTools)

theme_set(cowplot::theme_cowplot())

LW <- read_excel("../data/Earth_Land_Water.xlsx") |>
  mutate(
    C_Land = cumsum(Land),
    C_Water = cumsum(Water),
    `Percent Water` = Water / (Water + Land),
    n_Trials = Water + Land,
    C_n_Trials = cumsum(n_Trials),
    `Cumulative Percent Water` = C_Water / (C_Water + C_Land),
    Sample = 1:n()
  ) |>
  relocate(Sample) |>
  rowwise() |>
  mutate(
    CI_lower = BinomCI(Water, n_Trials)[1, 2],
    CI_upper = BinomCI(Water, n_Trials)[1, 3],
    C_CI_lower = BinomCI(C_Water, C_n_Trials)[1, 2],
    C_CI_upper = BinomCI(C_Water, C_n_Trials)[1, 3]
  )

LW |> as.data.frame()

ggplot(LW, aes(x = Sample, y = `Cumulative Percent Water`)) +
  geom_point(size = 3) +
  geom_line(color = "firebrick4", linewidth = 1) +
  scale_y_continuous(limits = c(0, 1))

BinomCI(118, 118 + 78, conf.level = 0.95, method = "clopper-pearson")


ggplot(
  data = tibble(
    Successes = 0:18,
    Probability = dbinom(0:18, 18, prob = 0.6020408)
  ),
  aes(Successes, Probability)
) +
  geom_bar(stat = "identity", fill = "firebrick4") +
  scale_x_continuous(breaks = 0:18) +
  labs(x = "n Successes from 18 Trials (Pr = 0.602)")

##############################################################################

ggplot(data = LW) +
  geom_bar(
    aes(x = Sample, y = `Percent Water`),
    stat = "identity",
    fill = "firebrick4",
    alpha = 0.5
  ) +
  geom_errorbar(
    aes(x = Sample, y = `Percent Water`, ymin = CI_lower, ymax = CI_upper),
    width = 0.25
  ) +
  scale_x_continuous(breaks = 1:8)


ggplot(data = LW) +
  geom_bar(
    aes(x = Sample, y = `Cumulative Percent Water`),
    stat = "identity",
    fill = "firebrick4",
    alpha = 0.5
  ) +
  geom_errorbar(
    aes(x = Sample, y = `Percent Water`, ymin = C_CI_lower, ymax = C_CI_upper),
    width = 0.25
  ) +
  scale_x_continuous(breaks = 1:8)

##############################################################################

n_samples <- 10000
sample_size <- 1

LW <- tibble(
  Sample = 1:n_samples,
  Water = rbinom(n = n_samples, size = sample_size, prob = 0.6020408),
  Land = sample_size - Water,
  C_Land = cumsum(Land),
  C_Water = cumsum(Water),
  `Cumulative Percent Water` = C_Water / (C_Water + C_Land)
)


message("Samples: ", max(LW$C_Land) + max(max(LW$C_Water)))

ggplot(LW, aes(x = Sample, y = `Cumulative Percent Water`)) +
  geom_line(color = "firebrick4", linewidth = 2) +
  scale_y_continuous(limits = c(0, 1))

##############################################################################

BinomCI(81, 100, conf.level = 0.95, method = "clopper-pearson")
