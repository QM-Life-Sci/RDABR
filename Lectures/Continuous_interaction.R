library(tidyverse)
library(colorspace)
library(cowplot)
theme_set(theme_cowplot())

set.seed(374362)

b1 <- 5
b2 <- -5
b3 <- -20

dat <- tibble(
  x1 = rnorm(100),
  x2 = rnorm(100),
  yy = b1 * x1 + b2 * x2 + b3 * x1 * x2 + rnorm(100, 1, 4)
)


dat |>
  ggplot(aes(x1, yy, color = x2)) +
  geom_point() +
  scale_color_continuous_diverging(palette = "Purple-Green")


library(tidyverse)
library(glue)

set.seed(2389472)
n <- 500

d <- tibble(
  age = round(runif(n, 25, 75)),
  calcium = round(rnorm(n, mean = 900, sd = 400)),
  vitamin_d = round(rnorm(n, mean = 60, sd = 25), 1)
) |>
  mutate(
    calcium = pmax(calcium, 100),
    vitamin_d = pmax(vitamin_d, 10)
  )

d <- d |>
  mutate(
    bmd_raw = 0.95 +
      0.00005 * calcium + # increase
      0.0008 * vitamin_d + # increase
      0.00001 * calcium * vitamin_d - # Ca x VitD
      0.004 * age + # BMD decrease with age
      rnorm(n, mean = 0, sd = 0.1),
    bmd = as.numeric(scale(bmd_raw)) # Z-score
  )

fm <- lm(bmd ~ calcium * vitamin_d + age, data = d)
summary(fm)

vd_cuts <- quantile(d$vitamin_d, probs = c(0.25, 0.50, 0.75))

grid <- crossing(
  calcium = seq(min(d$calcium), max(d$calcium), length.out = 100),
  vitamin_d = vd_cuts,
  age = median(d$age)
)

grid <- grid |>
  mutate(
    predicted_bmd = predict(fm, newdata = grid),
    vitamin_d_level = factor(
      vitamin_d,
      levels = vd_cuts,
      labels = glue(
        "{c('Low', 'Median', 'High')} ({round(vd_cuts)} nmol/L)"
      )
    )
  )

ggplot(grid, aes(x = calcium, y = predicted_bmd, color = vitamin_d_level)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Dietary Calcium (mg/dL)",
    y = "Standardized Femoral Neck BMD (Z)",
    color = "Serum Vitamin D"
  ) +
  theme_classic()
