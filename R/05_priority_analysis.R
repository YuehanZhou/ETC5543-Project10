library(tidyverse)

# Read master dataset
master_psp <- read_csv(
  "data/processed/master_psp.csv"
)

# Create focal-species presence indicator
priority_analysis <- master_psp |>
  mutate(
    focal_species_present =
      golden_sun_moth_records > 0 |
      growling_grass_frog_records > 0
  )

# Calculate preliminary thresholds
development_growth_median <- median(
  priority_analysis$growth_ha,
  na.rm = TRUE
)

development_share_median <- median(
  priority_analysis$urban_share_change,
  na.rm = TRUE
)

habitat_median <- median(
  priority_analysis$high_habitat_percent,
  na.rm = TRUE
)

# Create preliminary priority classification
priority_analysis <- priority_analysis |>
  mutate(
    high_development =
      growth_ha > development_growth_median |
      urban_share_change > development_share_median,
    
    high_biodiversity =
      high_habitat_percent > habitat_median |
      focal_species_present,
    
    no_bcs_protection =
      bcs_percent == 0,
    
    priority_area =
      high_development &
      high_biodiversity &
      no_bcs_protection
  )

# Check how many precincts meet each condition
priority_summary <- priority_analysis |>
  summarise(
    high_development = sum(high_development),
    high_biodiversity = sum(high_biodiversity),
    no_bcs_protection = sum(no_bcs_protection),
    priority_area = sum(priority_area)
  )

priority_summary

# Create preliminary priority precinct table
priority_precincts <- priority_analysis |>
  filter(priority_area) |>
  select(
    psp_name,
    lga,
    growth_ha,
    urban_share_change,
    high_habitat_percent,
    bcs_percent,
    golden_sun_moth_records,
    growling_grass_frog_records
  ) |>
  arrange(desc(growth_ha))

priority_precincts

# Show full priority table
priority_precincts |>
  print(n = Inf, width = Inf)

# Show why each precinct was selected
priority_conditions <- priority_analysis |>
  filter(priority_area) |>
  select(
    psp_name,
    growth_ha,
    urban_share_change,
    high_habitat_percent,
    focal_species_present,
    bcs_percent,
    high_development,
    high_biodiversity,
    no_bcs_protection
  ) |>
  arrange(desc(growth_ha))

priority_conditions |>
  print(n = Inf, width = Inf)