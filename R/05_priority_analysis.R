library(tidyverse)
library(sf)

# 1. Read master dataset
master_psp <- read_csv(
  "data/processed/master_psp.csv"
)

# 2. Create focal-species presence indicator
priority_analysis <- master_psp |>
  mutate(
    focal_species_present =
      golden_sun_moth_records > 0 |
      growling_grass_frog_records > 0
  )

# 3. Calculate screening thresholds
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

screening_thresholds <- tibble(
  development_growth_median =
    development_growth_median,
  development_share_median =
    development_share_median,
  habitat_median =
    habitat_median
)

screening_thresholds

# 4. Create ecological screening indicators
priority_analysis <- priority_analysis |>
  mutate(
    # Development pressure
    high_development =
      growth_ha > development_growth_median |
      urban_share_change > development_share_median,
    
    # Ecological value
    high_ecological_value =
      high_habitat_percent > habitat_median |
      focal_species_present,
    
    # No mapped BCS coverage
    no_bcs_coverage =
      bcs_percent == 0,
    
    # Baseline priority
    priority_area =
      high_development &
      high_ecological_value &
      no_bcs_coverage
  )

# 5. Summarise ecological screening results
priority_summary <- priority_analysis |>
  summarise(
    total_precincts = n(),
    high_development = sum(high_development),
    high_ecological_value = sum(high_ecological_value),
    no_bcs_coverage = sum(no_bcs_coverage),
    priority_precincts = sum(priority_area)
  )

priority_summary

# 6. Create ecological priority precinct table
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

# 7. Inspect priority conditions
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
    high_ecological_value,
    no_bcs_coverage
  ) |>
  arrange(psp_name)

stopifnot(
  !anyDuplicated(priority_analysis$psp_name),
  !anyNA(priority_analysis$high_development),
  !anyNA(priority_analysis$high_ecological_value),
  !anyNA(priority_analysis$priority_area)
)

# 8. Save ecological screening results
write_csv(
  priority_analysis,
  "data/processed/priority_analysis.csv"
)

# 9. Development pressure and habitat value
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
    aes(colour = priority_area),
    size = 3
  ) +
  scale_colour_discrete(
    name = "Priority precinct",
    labels = c("No", "Yes")
  ) +
  labs(
    title = "Development pressure and habitat value across PSP/MSA precincts",
    subtitle = "Dashed lines show median thresholds used in the baseline ecological screening",
    x = "Change in urban land share (percentage points)",
    y = "High-value habitat (%)",
    shape = "Focal species present",
    caption = "Development pressure also includes above-median absolute urban growth; priority status additionally requires no mapped BCS coverage."
  ) +
  theme_minimal()

print(priority_scatter)

# 10. Read spatial datasets
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

# 11. Prepare five-LGA study area
study_lga <- lga_boundary |>
  filter(
    LGA_NAME25 %in% c(
      "Casey",
      "Hume",
      "Melton",
      "Whittlesea",
      "Wyndham"
    )
  )

# 12. Standardise PSP names
psp_boundary <- psp_boundary |>
  mutate(
    PSP_NAME = case_when(
      PSP_NAME == "Quandong PSP (Council-led)" ~ "Quandong PSP",
      PSP_NAME == "Croskell" ~ "Croskell (Casey Central)",
      TRUE ~ PSP_NAME
    )
  )

# 13. Join MSA attributes to PSP polygons
msa_spatial <- msa |>
  left_join(
    psp_boundary,
    by = c("psp_name" = "PSP_NAME")
  ) |>
  st_as_sf() |>
  filter(
    !st_is_empty(geometry),
    psp_name %in% priority_analysis$psp_name
  )

# 14. Transform to VicGrid
msa_vicgrid <- st_transform(
  msa_spatial,
  3111
)

study_lga_vicgrid <- st_transform(
  study_lga,
  3111
)

# 15. Create study area
study_area <- study_lga_vicgrid |>
  st_union()

# 16. Clip PSP/MSA precincts to study area
msa_study <- suppressWarnings(
  st_intersection(
    msa_vicgrid,
    study_area
  )
) |>
  group_by(psp_name) |>
  summarise(
    lga = first(lga),
    .groups = "drop"
  )

stopifnot(
  n_distinct(msa_study$psp_name) == 101,
  setequal(
    msa_study$psp_name,
    priority_analysis$psp_name
  )
)

# 17. Join ecological screening results to spatial boundaries
priority_map <- msa_study |>
  left_join(
    priority_analysis |>
      select(
        psp_name,
        priority_area
      ),
    by = "psp_name"
  )

# 18. Create map groups
priority_map <- priority_map |>
  mutate(
    priority_group = case_when(
      priority_area ~ "Ecological priority precinct",
      TRUE ~ "Other study precinct"
    )
  )

# 20. Number priority precincts alphabetically
priority_labels <- priority_map |>
  filter(priority_area) |>
  arrange(psp_name) |>
  mutate(
    priority_id = row_number()
  )

# 21. Create label positions for priority precincts
priority_label_points <- suppressWarnings(
  st_point_on_surface(
    st_geometry(priority_labels)
  )
)

priority_label_points_sf <- st_sf(
  priority_labels |>
    st_drop_geometry(),
  geometry = priority_label_points
)

# 22. Transform map and labels to longitude / latitude
priority_map_ll <- st_transform(
  priority_map,
  4326
)

study_lga_ll <- st_transform(
  study_lga_vicgrid,
  4326
)

priority_label_points_ll <- st_transform(
  priority_label_points_sf,
  4326
)

# 23. Extract coordinates for priority labels
label_coordinates <- st_coordinates(
  priority_label_points_ll
)

priority_label_xy <- priority_label_points_ll |>
  st_drop_geometry() |>
  mutate(
    longitude = label_coordinates[, 1],
    latitude = label_coordinates[, 2]
  )

# Small manual adjustments for overlapping labels
priority_label_xy <- priority_label_xy |>
  mutate(
    longitude = case_when(
      priority_id == 10 ~ longitude - 0.01,
      TRUE ~ longitude
    ),
    latitude = case_when(
      priority_id == 10 ~ latitude + 0.006,
      TRUE ~ latitude
    )
  )

# 24. Create LGA label positions
lga_label_points <- suppressWarnings(
  st_point_on_surface(
    study_lga_ll
  )
)

lga_coordinates <- st_coordinates(
  lga_label_points
)

lga_label_xy <- lga_label_points |>
  st_drop_geometry() |>
  mutate(
    longitude = lga_coordinates[, 1],
    latitude = lga_coordinates[, 2]
  )

# 25. Create numbered priority precinct key
priority_key_simple <- priority_labels |>
  st_drop_geometry() |>
  select(
    priority_id,
    psp_name,
    lga
  )

# 26. Create two-column precinct key
priority_key_left <- priority_key_simple |>
  filter(priority_id <= 8) |>
  mutate(
    key_label = paste0(
      priority_id,
      "  ",
      str_wrap(psp_name, width = 23)
    )
  ) |>
  pull(key_label) |>
  paste(collapse = "\n")

priority_key_right <- priority_key_simple |>
  filter(priority_id >= 9) |>
  mutate(
    key_label = paste0(
      priority_id,
      "  ",
      str_wrap(psp_name, width = 23)
    )
  ) |>
  pull(key_label) |>
  paste(collapse = "\n")


# 27. Use full five-LGA extent
map_bbox <- st_bbox(study_lga_ll)

# 28. Final presentation map
priority_map_plot <- ggplot() +
  # PSP/MSA precincts
  geom_sf(
    data = priority_map_ll,
    aes(fill = priority_group),
    colour = "white",
    linewidth = 0.2
  ) +
  # LGA boundaries
  geom_sf(
    data = study_lga_ll,
    fill = NA,
    colour = "black",
    linewidth = 0.6
  ) +
  # Priority precinct numbers
  geom_text(
    data = priority_label_xy,
    aes(
      x = longitude,
      y = latitude,
      label = priority_id
    ),
    size = 3.5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  # LGA labels
  geom_text(
    data = lga_label_xy,
    aes(
      x = longitude,
      y = latitude,
      label = LGA_NAME25
    ),
    size = 4,
    fontface = "bold",
    colour = "grey25",
    inherit.aes = FALSE
  ) +
  # Priority key title
  annotate(
    "text",
    x = as.numeric(map_bbox["xmax"]) + 0.05,
    y = as.numeric(map_bbox["ymax"]) - 0.02,
    label = "Priority precinct key",
    hjust = 0,
    vjust = 1,
    size = 3.5,
    fontface = "bold"
  ) +
  
  # Priority key left column
  annotate(
    "text",
    x = as.numeric(map_bbox["xmax"]) + 0.05,
    y = as.numeric(map_bbox["ymax"]) - 0.06,
    label = priority_key_left,
    hjust = 0,
    vjust = 1,
    size = 2.4,
    lineheight = 1.15
  ) +
  # Priority key right column
  annotate(
    "text",
    x = as.numeric(map_bbox["xmax"]) + 0.48,
    y = as.numeric(map_bbox["ymax"]) - 0.06,
    label = priority_key_right,
    hjust = 0,
    vjust = 1,
    size = 2.4,
    lineheight = 1.15
  ) +
  scale_fill_manual(
    values = c(
      "Ecological priority precinct" = "#C0392B",
      "Other study precinct" = "#ECECEC"
    ),
    name = NULL
  ) +
  labs(
    title = "Ecological priority screening across Melbourne's fringe growth areas",
    subtitle = "Precincts are screened using development pressure, habitat value, focal-species records and BCS coverage",
    x = "Longitude",
    y = "Latitude",
    caption = "Screening identifies precincts for further ecological and corridor assessment; it does not directly model fauna movement or connectivity."
  ) +
  coord_sf(
    xlim = c(
      as.numeric(map_bbox["xmin"]),
      as.numeric(map_bbox["xmax"]) + 0.82
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
      colour = "grey92",
      linewidth = 0.25
    ),
    panel.grid.minor = element_blank(),
    axis.title = element_text(
      size = 10
    ),
    axis.text = element_text(
      size = 9,
      colour = "grey40"
    ),
    legend.position = "bottom",
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    plot.caption = element_text(
      colour = "grey40",
      size = 8,
      margin = margin(t = 8)
    )
  )

print(priority_map_plot)