library(tidyverse)
library(readxl)
library(DescTools)

theme_set(cowplot::theme_cowplot())

LW <- read_excel("../data/Earth_Land_Water.xlsx") |> 
  mutate(C_Land = cumsum(Land),
         C_Water = cumsum(Water),
         `Percent Water` = C_Water / (C_Water + C_Land),
         Sample = 1:n())

LW

ggplot(LW, aes(x = Sample, y = `Percent Water`)) +
  geom_line(color = "firebrick4", linewidth = 2) +
  scale_y_continuous(limits = c(0, 1))

################################################################

n_samples <- 10
sample_size <- 10

LW <- tibble(Sample = 1:n_samples,
             Water = rbinom(n = n_samples, size = sample_size, prob = 0.71),
             Land = sample_size - Water,
             C_Land = cumsum(Land),
             C_Water = cumsum(Water),
             `Percent Water` = C_Water / (C_Water + C_Land))


message("Samples: ", max(LW$C_Land) + max(max(LW$C_Water)))

ggplot(LW, aes(x = Sample, y = `Percent Water`)) +
  geom_line(color = "firebrick4", linewidth = 2) +
  scale_y_continuous(limits = c(0, 1))

################################################################

BinomCI(9, 10,
        conf.level = 0.95,
        method = "clopper-pearson")

BinomCI(81, 100,
        conf.level = 0.95,
        method = "clopper-pearson")
