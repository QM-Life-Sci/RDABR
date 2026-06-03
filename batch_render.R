# Lecture slides
library(quarto)
options(warn=2)

## Install packages
pkgs <- c(
  "AICcmodavg",
  "broom",
  "colorspace",
  "cowplot",
  "cvTools",
  "DescTools",
  "effectsize",
  "estimatr",
  "factoextra",
  "GGally",
  "ggExtra",
  "ggforce",
  "ggpubr",
  "ggrepel",
  "ggsci",
  "ggstatsplot",
  "ggtext",
  "glue",
  "gt",
  "ICC",
  "knitr",
  "latex2exp",
  "magick",
  "modelsummary",
  "multcomp",
  "mvtnorm",
  "paletteer",
  "palmerpenguins",
  "patchwork",
  "performance",
  "plotly",
  "purrr",
  "pwr",
  "quarto",
  "readxl",
  "report",
  "Rmpfr",
  "see",
  "smatr",
  "swirl",
  "tidyplots",
  "tidyverse",
  "viridis",
  "wesanderson"
)

# Uncomment to install all packages:
# pak::pak(pkgs)


qmds <- list.files(pattern = "^[0-9]*-[1-6].*qmd$",
                   path = "Lectures/",
                   full.names = TRUE)

for (qq in qmds) {
  quarto_render(input = qq,
                metadata = list("self-contained" = "true"))
}

## Problem sets

qmds <- list.files(pattern = "^PS_[0-9]*[_Key]*.qmd$",
                   path = "Problem_Sets",
                   full.names = TRUE)

for (qq in qmds) {
  quarto_render(input = qq,
                metadata = list("self-contained" = "true"))
}
