library(tidyverse)
library(sf)
library(ggplot2)

# Population growth
population_growth <- population |>
  filter(year %in% c(2001, 2025)) |>
  select(council, year, population) |>
  pivot_wider(
    names_from = year,
    values_from = population
  ) |>
  mutate(
    growth = `2025` - `2001`,
    growth_percent = growth / `2001` * 100
  )

population_growth

# Join population to LGA
study_lga <- study_lga |>
  mutate(
    lga_code_2025 = as.numeric(LGA_CODE25)
  )

population_map <- study_lga |>
  left_join(
    population,
    by = "lga_code_2025"
  )

glimpse(population_map)

# Map study area
ggplot(study_lga) +
  geom_sf() +
  geom_sf_text(aes(label = LGA_NAME25)) +
  labs(
    title = "Study LGAs",
    x = NULL,
    y = NULL
  ) +
  theme_minimal()