library(tidyverse)
library(palmerpenguins)
library(modelsummary)
library(report)

glimpse(penguins)

penguins <- penguins |> 
  mutate(year = factor(year))

pp <- penguins |> 
  drop_na(body_mass_g, sex)

mean(pp$body_mass_g)
median(pp$body_mass_g)


set.seed(3438)

iters <- 10000

means <- numeric(length = iters)

# Size of sample
n <- 10

# Bootstrapped mean

for (ii in 1:iters) {
  
  # Sample from body mass
  s <- sample(pp$body_mass_g, size = n)
  
  # Mean of s
  s_mean <- mean(s)
  
  # Assign to means
  means[ii] <- s_mean
}

means

mean(means)


ggplot(pp, aes(flipper_length_mm, body_mass_g, color = species)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = FALSE, method = "lm") +
  facet_grid(sex ~ .)

fm <- lm(body_mass_g ~ flipper_length_mm + sex + species, data = pp)
summary(fm)


gt <- pp |> filter(species == "Gentoo")

ggplot(gt, aes(flipper_length_mm, body_mass_g, color = sex)) +
  geom_point() +
  geom_smooth(formula = y ~ x, se = FALSE, method = "lm")

# "Multiple regression" Linear model

fm_gt <- lm(body_mass_g ~ flipper_length_mm + sex, data = gt)
summary(fm_gt)


library(report)

report(fm)



