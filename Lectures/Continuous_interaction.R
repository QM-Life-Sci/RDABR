library(tidyverse)
library(colorspace)
library(cowplot)
theme_set(theme_cowplot())

set.seed(374362)

b1 <- 5
b2 <- -5
b3 <- -20

dat <- tibble(x1 = rnorm(100),
              x2 = rnorm(100),
              yy = b1*x1 + b2*x2 + b3*x1*x2 + rnorm(100,1,4)
)


dat |>
  ggplot(aes(x1,yy, color=x2)) +
  geom_point() +
  scale_color_continuous_diverging(palette = "Purple-Green")

