# Lecture slides
library(quarto)
options(warn=2)

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
