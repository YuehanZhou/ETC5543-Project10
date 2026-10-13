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
  arrange(psp_name)

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
  arrange(psp_name)

priority_conditions |>
  print(n = Inf, width = Inf)

# Save final priority results
write_csv(
  priority_analysis,
  "data/processed/priority_analysis.csv"
)

# Development pressure and biodiversity importance
priority_scatter <- ggplot(
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

print(priority_scatter)

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
        priority_area,
        growth_ha
      ),
    by = "psp_name"
  )

# Number priority precincts alphabetically
priority_labels <- priority_map |>
  filter(priority_area) |>
  arrange(psp_name) |>
  mutate(
    priority_id = row_number()
  )

# Create label positions while still using projected CRS
priority_label_points <- st_point_on_surface(
  priority_labels
)

# Transform map and labels to longitude/latitude
priority_map_ll <- st_transform(
  priority_map,
  4326
)

study_lga_ll <- st_transform(
  study_lga_vicgrid,
  4326
)

priority_label_points_ll <- st_transform(
  priority_label_points,
  4326
)

# Numbered key
priority_key_simple <- priority_labels |>
  st_drop_geometry() |>
  select(
    priority_id,
    psp_name,
    lga
  )

priority_key_simple |>
  print(n = Inf, width = Inf)

# Create text for the numbered precinct key
priority_key_text <- priority_key_simple |>
  mutate(
    key_label = paste0(priority_id, "  ", psp_name)
  ) |>
  pull(key_label) |>
  paste(collapse = "\n")

# Get map extent
map_bbox <- st_bbox(priority_map_ll)

# Final numbered priority map with precinct key
priority_map_plot <- ggplot() +
  geom_sf(
    data = priority_map_ll,
    aes(fill = priority_area),
    colour = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = study_lga_ll,
    fill = NA,
    colour = "black",
    linewidth = 0.5
  ) +
  geom_sf_text(
    data = priority_label_points_ll,
    aes(label = priority_id),
    size = 3.5,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = as.numeric(map_bbox["xmax"]) + 0.06,
    y = as.numeric(map_bbox["ymax"]) - 0.02,
    label = paste0(
      "Priority precinct key\n\n",
      priority_key_text
    ),
    hjust = 0,
    vjust = 1,
    size = 3.2,
    lineheight = 1.15
  ) +
  scale_fill_discrete(
    name = "Priority precinct",
    labels = c("No", "Yes")
  ) +
  labs(
    title = "Priority PSP/MSA precincts across Melbourne's fringe growth areas",
    subtitle = "Numbers identify priority precincts",
    x = "Longitude",
    y = "Latitude",
    caption = "Numbers are identifiers only and do not indicate ranking."
  ) +
  coord_sf(
    xlim = c(
      as.numeric(map_bbox["xmin"]),
      as.numeric(map_bbox["xmax"]) + 0.55
    ),
    ylim = c(
      as.numeric(map_bbox["ymin"]),
      as.numeric(map_bbox["ymax"])
    ),
    datum = st_crs(4326),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

print(priority_map_plot)