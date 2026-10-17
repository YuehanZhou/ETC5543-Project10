library(tidyverse)

species <- read_csv(
  "data/processed/focal_species_precincts.csv"
)

connectivity <- read_csv(
  "data/processed/connectivity_results.csv"
)

names(species)
names(connectivity)