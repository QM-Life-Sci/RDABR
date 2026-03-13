library(tidyverse)
library(palmerpenguins)

p2 <- penguins |> select(species, bill_length_mm, bill_depth_mm) |> 
  drop_na()

# filter()

p2 |> filter(bill_length_mm > 40)
p2 |> filter(species == "Adelie")
p2 |> filter(species != "Adelie" & bill_length_mm > 50)

# slice() slice_...()

p2 |> arrange(bill_length_mm)
p2 |> arrange(desc(bill_length_mm)) |> slice(1:5)

p2 |> slice_head(n = 5)
p2 |> slice_tail(n = 5)

p2 |> slice_max(bill_length_mm, n = 5)

p2 |> slice_sample(n = 20)
p2 |> slice_sample(prop = 0.25)

p2 |> 
  group_by(species) |> summarize(n = n(), 
                                 mean_bill = mean(bill_length_mm), 
                                 t_crit = abs(qt(0.025, n - 1)))

abs(qt(0.025, 10))
qt(0.975, 10)
qt(1  - 0.025, 10)

# Upper: Mean + t_crit * SEM
# Lower: Mean - t_crit * SEM

fm <- lm(bill_length_mm ~ bill_depth_mm, p2)
summary(fm)

confint(fm)

ggplot(p2, aes(y = bill_length_mm, x = bill_depth_mm))+
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se=FALSE)

ggplot(p2, aes(y = bill_length_mm, x = bill_depth_mm, color = species))+
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se=FALSE)
