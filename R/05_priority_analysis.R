library(tidyverse)
library(sf)

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

# Calculate thresholds
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

# Create final priority classification
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

# Summarise priority classification
priority_summary <- priority_analysis |>
  summarise(
    total_precincts = n(),
    high_development = sum(high_development),
    high_biodiversity = sum(high_biodiversity),
    no_bcs_protection = sum(no_bcs_protection),
    priority_precincts = sum(priority_area)
  )

priority_summary

# Create final priority precinct table
priority_precincts <- priority_analysis |>
  filter(priority_area) |>
  select(
    psp_name,
    lga,
    growth_ha,
    urban_share_change,
    high_habitat_percent,
    focal_species_present,
    bcs_percent,
    golden_sun_moth_records,
    growling_grass_frog_records
  ) |>
  arrange(desc(growth_ha))

priority_precincts |>
  print(n = Inf, width = Inf)

# Show why each priority precinct was selected
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

# Save final priority results
write_csv(
  priority_analysis,
  "data/processed/priority_analysis.csv"
)

write_csv(
  priority_precincts,
  "data/processed/priority_precincts.csv"
)

# Development pressure and biodiversity importance
ggplot(
  priority_analysis,
  aes(
    x = urban_share_change,
    y = high_habitat_percent,
    shape = focal_species_present
  )
) +
  geom_vline(
    xintercept = development_share_median,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = habitat_median,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_point(
    aes(alpha = priority_area),
    size = 3
  ) +
  scale_shape_discrete(
    name = "Focal species present",
    labels = c("No", "Yes")
  ) +
  scale_alpha_discrete(
    name = "Priority precinct",
    labels = c("No", "Yes")
  ) +
  labs(
    title = "Development pressure and biodiversity importance across PSP/MSA precincts",
    x = "Change in urban land share (percentage points)",
    y = "High-value habitat (%)"
  ) +
  theme_minimal()

# Read MSA attributes
msa <- read_csv(
  "data/meeting1/msa_precinct_attributes.csv"
)

# Read PSP boundary
psp_boundary <- st_read(
  "data/PSP_Boundary/PSP_Boundary.geojson",
  quiet = TRUE
)

# Read LGA boundary
lga_boundary <- st_read(
  "data/LGA_2025_AUST_GDA2020/LGA_2025_AUST_GDA2020.shp",
  quiet = TRUE
)

# Keep only the five study LGAs
study_lga <- lga_boundary |>
  filter(LGA_NAME25 %in% c(
    "Casey",
    "Hume",
    "Melton",
    "Whittlesea",
    "Wyndham"
  ))

# Standardise PSP names
psp_boundary <- psp_boundary |>
  mutate(
    PSP_NAME = case_when(
      PSP_NAME == "Quandong PSP (Council-led)" ~ "Quandong PSP",
      PSP_NAME == "Croskell" ~ "Croskell (Casey Central)",
      TRUE ~ PSP_NAME
    )
  )

# Join MSA attributes to PSP polygons
msa_spatial <- msa |>
  left_join(
    psp_boundary,
    by = c("psp_name" = "PSP_NAME")
  ) |>
  st_as_sf() |>
  filter(!st_is_empty(geometry))

# Transform to VicGrid
msa_vicgrid <- st_transform(
  msa_spatial,
  3111
)

study_lga_vicgrid <- st_transform(
  study_lga,
  3111
)

# Create the five-LGA study area
study_area <- study_lga_vicgrid |>
  st_union()

# Clip PSP/MSA precincts to the study area
msa_study <- msa_vicgrid |>
  st_intersection(study_area)

# Join final priority classification to spatial boundaries
priority_map <- msa_study |>
  left_join(
    priority_analysis |>
      select(
        psp_name,
        priority_area
      ),
    by = "psp_name"
  )

# Final priority map
ggplot() +
  geom_sf(
    data = priority_map,
    aes(fill = priority_area),
    colour = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = study_lga_vicgrid,
    fill = NA,
    linewidth = 0.5
  ) +
  scale_fill_discrete(
    name = "Priority precinct",
    labels = c("No", "Yes")
  ) +
  labs(
    title = "Priority PSP/MSA precincts across Melbourne's fringe growth areas"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank()
  )