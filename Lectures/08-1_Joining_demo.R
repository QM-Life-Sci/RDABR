library(tidyverse)

island_data <- tibble(
  island = as.character(levels(penguins$island)),
  latitude = c(-64.766667, -65.433333, -64.733333),
  longitude = c(-64.083333, -65.5, -64.233333)
)

str(penguins)
