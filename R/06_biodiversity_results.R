library(tidyverse)
library(sf)
library(terra)

# Read the combined precinct dataset
master_psp <- read_csv(
  "data/processed/master_psp.csv"
)

# Keep variables relevant to biodiversity analysis
biodiversity_psp <- master_psp |>
  select(
    psp_name,
    lga,
    high_habitat_percent,
    bcs_percent,
    golden_sun_moth_records,
    growling_grass_frog_records
  ) |>
  mutate(
    golden_sun_moth_present =
      golden_sun_moth_records > 0,
    
    growling_grass_frog_present =
      growling_grass_frog_records > 0,
    
    focal_species_present =
      golden_sun_moth_present |
      growling_grass_frog_present
  )

# Identify precincts with the highest high-value habitat coverage
top_habitat_precincts <- biodiversity_psp |>
  arrange(desc(high_habitat_percent)) |>
  select(
    psp_name,
    lga,
    high_habitat_percent,
    bcs_percent,
    focal_species_present
  ) |>
  slice_head(n = 15)

# Identify precincts with focal-species records
species_precincts <- biodiversity_psp |>
  filter(focal_species_present) |>
  select(
    psp_name,
    lga,
    golden_sun_moth_records,
    growling_grass_frog_records,
    high_habitat_percent,
    bcs_percent
  ) |>
  arrange(desc(golden_sun_moth_records))

# Load Habitat Value raster
habitat <- rast(
  "data/habitat_value/habitat_value_75m_20241024_vg2020.tif"
)

# Transform precincts and BCS polygons to match the habitat raster CRS
msa_habitat_crs <- st_transform(
  msa_study,
  7899
)

bcs_habitat_crs <- st_transform(
  bcs,
  7899
)

# Create high-value habitat layer
# Habitat Value >= 70 is treated as high-value habitat
high_habitat <- habitat >= 70

# Convert BCS polygons to the same raster grid
bcs_raster <- rasterize(
  vect(bcs_habitat_crs),
  habitat,
  field = 1,
  background = 0
)

# High-value habitat inside BCS Conservation Areas
high_habitat_inside_bcs <-
  high_habitat * bcs_raster

# High-value habitat outside BCS Conservation Areas
high_habitat_outside_bcs <-
  high_habitat * (1 - bcs_raster)

# Convert precinct polygons for terra
precinct_vect <- vect(msa_habitat_crs)

# Each raster cell is 75 m x 75 m
cell_area_ha <- (75 * 75) / 10000

# Calculate total high-value habitat by precinct
total_high <- extract(
  high_habitat,
  precinct_vect,
  fun = sum,
  exact = TRUE,
  na.rm = TRUE
)

# Calculate high-value habitat inside BCS by precinct
inside_high <- extract(
  high_habitat_inside_bcs,
  precinct_vect,
  fun = sum,
  exact = TRUE,
  na.rm = TRUE
)

# Calculate high-value habitat outside BCS by precinct
outside_high <- extract(
  high_habitat_outside_bcs,
  precinct_vect,
  fun = sum,
  exact = TRUE,
  na.rm = TRUE
)

# Create precinct-level conservation-gap dataset
protection_gap <- tibble(
  psp_name = msa_habitat_crs$psp_name,
  
  high_habitat_ha =
    total_high[[2]] * cell_area_ha,
  
  high_habitat_inside_bcs_ha =
    inside_high[[2]] * cell_area_ha,
  
  high_habitat_outside_bcs_ha =
    outside_high[[2]] * cell_area_ha
) |>
  mutate(
    high_habitat_outside_bcs_percent =
      if_else(
        high_habitat_ha > 0,
        high_habitat_outside_bcs_ha /
          high_habitat_ha * 100,
        NA_real_
      )
  )

# Add focal-species information
biodiversity_gap_results <- protection_gap |>
  left_join(
    biodiversity_psp |>
      select(
        psp_name,
        lga,
        golden_sun_moth_records,
        growling_grass_frog_records,
        focal_species_present
      ),
    by = "psp_name"
  ) |>
  arrange(desc(high_habitat_outside_bcs_ha))

# Create overall conservation-gap summary
overall_gap_summary <- protection_gap |>
  summarise(
    total_high_habitat_ha =
      sum(high_habitat_ha, na.rm = TRUE),
    
    high_habitat_inside_bcs_ha =
      sum(high_habitat_inside_bcs_ha, na.rm = TRUE),
    
    high_habitat_outside_bcs_ha =
      sum(high_habitat_outside_bcs_ha, na.rm = TRUE)
  ) |>
  mutate(
    outside_bcs_percent =
      high_habitat_outside_bcs_ha /
      total_high_habitat_ha * 100
  )

# Identify Top 10 precincts with the largest amount of high-value habitat outside BCS
top_gap_precincts <- biodiversity_gap_results |>
  arrange(desc(high_habitat_outside_bcs_ha)) |>
  select(
    psp_name,
    lga,
    high_habitat_ha,
    high_habitat_inside_bcs_ha,
    high_habitat_outside_bcs_ha,
    high_habitat_outside_bcs_percent,
    golden_sun_moth_records,
    growling_grass_frog_records,
    focal_species_present
  ) |>
  slice_head(n = 10) |>
  mutate(
    rank = row_number()
  )

# Create bar chart
gap_bar <- ggplot(
  top_gap_precincts,
  aes(
    x = reorder(
      psp_name,
      high_habitat_outside_bcs_ha
    ),
    y = high_habitat_outside_bcs_ha
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title =
      "High-value habitat outside BCS Conservation Areas",
    x = NULL,
    y = "High-value habitat outside BCS (ha)"
  ) +
  theme_minimal()

print(gap_bar)

ggsave(
  "figures/conservation_gap_bar.png",
  plot = gap_bar,
  width = 8,
  height = 5,
  dpi = 300
)

# Join Top 10 results to precinct boundaries
biodiversity_gap_map <- msa_habitat_crs |>
  left_join(
    top_gap_precincts |>
      select(
        psp_name,
        rank
      ),
    by = "psp_name"
  ) |>
  mutate(
    top_gap = !is.na(rank)
  )

# Create shorter labels for the map
label_points <- biodiversity_gap_map |>
  filter(top_gap) |>
  mutate(
    short_name = case_when(
      psp_name == "Ravenhall (Quarry Site)" ~
        "Ravenhall",
      
      psp_name == "Sunbury South" ~
        "Sunbury Sth",
      
      psp_name == "Rockbank South" ~
        "Rockbank Sth",
      
      psp_name == "Werribee Junction" ~
        "Werribee Jct",
      
      psp_name == "Cooper Street West" ~
        "Cooper St W",
      
      TRUE ~ psp_name
    ),
    
    short_label =
      paste(rank, short_name)
  ) |>
  st_point_on_surface()

# Create biodiversity protection-gap map
gap_map <- ggplot() +
  geom_sf(
    data = biodiversity_gap_map,
    aes(fill = top_gap),
    colour = "white",
    linewidth = 0.2
  ) +
  geom_sf_text(
    data = label_points,
    aes(label = short_label),
    size = 2.5
  ) +
  scale_fill_discrete(
    name = "Top 10 habitat outside BCS",
    labels = c(
      "No",
      "Yes"
    )
  ) +
  labs(
    title =
      "Precincts with the largest areas of high-value habitat outside BCS"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank()
  )

print(gap_map)

ggsave(
  "figures/conservation_gap_map.png",
  plot = gap_map,
  width = 10,
  height = 7,
  dpi = 300
)

# Save analysis results
write_csv(
  biodiversity_gap_results,
  "data/processed/biodiversity_gap_results.csv"
)

write_csv(
  top_gap_precincts,
  "data/processed/top_biodiversity_gap_precincts.csv"
)

write_csv(
  overall_gap_summary,
  "data/processed/biodiversity_gap_summary.csv"
)

write_csv(
  species_precincts,
  "data/processed/focal_species_precincts.csv"
)

write_csv(
  top_habitat_precincts,
  "data/processed/top_habitat_precincts.csv"
)


# Join focal-species information to precinct boundaries
species_map <- msa_habitat_crs |>
  left_join(
    biodiversity_psp |>
      select(
        psp_name,
        golden_sun_moth_present,
        growling_grass_frog_present
      ),
    by = "psp_name"
  ) |>
  mutate(
    species_group = case_when(
      golden_sun_moth_present &
        growling_grass_frog_present ~ "Both species",
      
      golden_sun_moth_present ~ "Golden Sun Moth",
      
      growling_grass_frog_present ~ "Growling Grass Frog",
      
      TRUE ~ "No focal-species record"
    )
  )

# Convert to standard latitude/longitude coordinates
species_map_ll <- st_transform(species_map, 4326)

# Plot focal-species occurrence
species_plot <- ggplot() +
  geom_sf(
    data = species_map_ll,
    aes(fill = species_group),
    colour = "white",
    linewidth = 0.2
  ) +
  labs(
    title = "Recent focal-species records across PSP/MSA precincts",
    x = "Longitude",
    y = "Latitude",
    fill = "Focal-species record"
  ) +
  theme_minimal()

print(species_plot)

ggsave(
  "figures/focal_species_map.png",
  plot = species_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# Create combined dataset for final group analysis / interactive explorer
explorer_data <- master_psp |>
  left_join(
    protection_gap |>
      select(
        psp_name,
        high_habitat_ha,
        high_habitat_inside_bcs_ha,
        high_habitat_outside_bcs_ha,
        high_habitat_outside_bcs_percent
      ),
    by = "psp_name"
  ) |>
  mutate(
    species_group = case_when(
      golden_sun_moth_records > 0 &
        growling_grass_frog_records > 0 ~ "Both species",
      
      golden_sun_moth_records > 0 ~ "Golden Sun Moth",
      
      growling_grass_frog_records > 0 ~ "Growling Grass Frog",
      
      TRUE ~ "No focal-species record"
    )
  )

stopifnot(
  nrow(explorer_data) == 101,
  n_distinct(explorer_data$psp_name) == 101,
  !anyDuplicated(explorer_data$psp_name)
)

write_csv(
  explorer_data,
  "data/processed/priority_explorer_data.csv"
)
