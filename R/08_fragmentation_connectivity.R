library(tidyverse)
library(sf)
library(terra)

# 1. Read spatial datasets
# Habitat Value raster
habitat <- rast(
  "data/habitat_value/habitat_value_75m_20241024_vg2020.tif"
)

# MSA / PSP attributes
msa <- read_csv(
  "data/meeting1/msa_precinct_attributes.csv"
)

# PSP boundaries
psp_boundary <- st_read(
  "data/PSP_Boundary/PSP_Boundary.geojson",
  quiet = TRUE
)

# LGA boundaries
lga_boundary <- st_read(
  "data/LGA_2025_AUST_GDA2020/LGA_2025_AUST_GDA2020.shp",
  quiet = TRUE
)

# 2. Prepare study area
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

# Standardise PSP names
psp_boundary <- psp_boundary |>
  mutate(
    PSP_NAME = case_when(
      PSP_NAME == "Quandong PSP (Council-led)" ~ "Quandong PSP",
      PSP_NAME == "Croskell" ~ "Croskell (Casey Central)",
      TRUE ~ PSP_NAME
    )
  )

# Join MSA attributes to PSP geometry
msa_spatial <- msa |>
  left_join(
    psp_boundary,
    by = c("psp_name" = "PSP_NAME")
  ) |>
  st_as_sf() |>
  filter(!st_is_empty(geometry))

# Transform PSPs to Habitat Value raster CRS
msa_habitat_crs <- st_transform(
  msa_spatial,
  crs(habitat)
)

study_lga_habitat_crs <- st_transform(
  study_lga,
  crs(habitat)
)

# Create five-LGA study area
study_area <- study_lga_habitat_crs |>
  st_union()

# Clip PSP/MSA polygons to study area
msa_study <- suppressWarnings(
  st_intersection(
    msa_habitat_crs,
    study_area
  )
)

# Keep one record per precinct where possible
msa_study <- msa_study |>
  group_by(psp_name) |>
  summarise(
    lga = first(lga),
    geometry = st_union(geometry),
    .groups = "drop"
  )

# 3. Prepare high-value habitat raster
study_area_vect <- vect(
  st_sf(geometry = study_area)
)

habitat_study <- crop(
  habitat,
  study_area_vect
)

habitat_study <- mask(
  habitat_study,
  study_area_vect
)

# High-value habitat = Habitat Value >= 70
# Non-high-value cells are set to NA
high_habitat <- ifel(
  habitat_study >= 70,
  1,
  NA
)

names(high_habitat) <- "high_habitat"

# 4. Identify continuous habitat patches
# 8-direction connectivity means diagonally touching habitat cells
# are treated as part of the same patch
habitat_patches <- patches(
  high_habitat,
  directions = 8
)

names(habitat_patches) <- "patch_id"

# Check number of habitat patches across the study area
patch_frequency <- freq(
  habitat_patches
)

nrow(patch_frequency)

# 5. Calculate raster cell area
# Habitat raster is projected, so approximate cell area is based
# on raster resolution
cell_area_ha <- prod(
  res(habitat_patches)
) / 10000

cell_area_ha

# 6. Extract habitat patches within each PSP/MSA precinct
patch_extract <- terra::extract(
  habitat_patches,
  vect(msa_study),
  touches = TRUE,
  cells = TRUE
)

# Add precinct names using the polygon ID returned by terra
patch_extract <- patch_extract |>
  mutate(
    psp_name = msa_study$psp_name[ID]
  ) |>
  filter(
    !is.na(patch_id)
  )

# 7. Calculate patch-level area within each precinct
patch_area_by_precinct <- patch_extract |>
  count(
    psp_name,
    patch_id,
    name = "habitat_cells"
  ) |>
  mutate(
    patch_area_ha =
      habitat_cells * cell_area_ha
  )

# 8. Fragmentation metrics by precinct
fragmentation_metrics <- patch_area_by_precinct |>
  group_by(psp_name) |>
  summarise(
    total_high_habitat_ha = sum(patch_area_ha, na.rm = TRUE),
    habitat_patch_count = n_distinct(patch_id),
    largest_patch_ha = max(patch_area_ha, na.rm = TRUE),
    mean_patch_ha = mean(patch_area_ha, na.rm = TRUE),
    median_patch_ha = median(patch_area_ha, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    largest_patch_share =
      largest_patch_ha / total_high_habitat_ha * 100
  )

# 9. Precinct area
precinct_area <- msa_study |>
  mutate(
    precinct_area_ha =
      as.numeric(st_area(geometry)) / 10000
  ) |>
  st_drop_geometry() |>
  select(
    psp_name,
    lga,
    precinct_area_ha
  ) |>
  distinct()

# 10. Add precincts with no high-value habitat
fragmentation_metrics <- precinct_area |>
  left_join(
    fragmentation_metrics,
    by = "psp_name"
  ) |>
  mutate(
    total_high_habitat_ha =
      replace_na(total_high_habitat_ha, 0),
    
    habitat_patch_count =
      replace_na(habitat_patch_count, 0),
    
    largest_patch_ha =
      replace_na(largest_patch_ha, 0),
    
    mean_patch_ha =
      replace_na(mean_patch_ha, 0),
    
    median_patch_ha =
      replace_na(median_patch_ha, 0),
    
    largest_patch_share =
      replace_na(largest_patch_share, 0),
    
    patch_density_100ha =
      if_else(
        precinct_area_ha > 0,
        habitat_patch_count / precinct_area_ha * 100,
        NA_real_
      )
  )

# 11. Inspect fragmentation results
fragmentation_metrics |>
  arrange(desc(patch_density_100ha)) |>
  print(
    n = 20,
    width = Inf
  )

fragmentation_metrics |>
  summarise(
    total_precincts = n(),
    
    precincts_with_high_habitat =
      sum(total_high_habitat_ha > 0),
    
    median_patch_density =
      median(
        patch_density_100ha[
          total_high_habitat_ha > 0
        ],
        na.rm = TRUE
      ),
    
    median_largest_patch_share =
      median(
        largest_patch_share[
          total_high_habitat_ha > 0
        ],
        na.rm = TRUE
      )
  )

# 12. Initial fragmentation indicator
patch_density_median <- median(
  fragmentation_metrics$patch_density_100ha[
    fragmentation_metrics$total_high_habitat_ha > 0
  ],
  na.rm = TRUE
)

largest_patch_share_median <- median(
  fragmentation_metrics$largest_patch_share[
    fragmentation_metrics$total_high_habitat_ha > 0
  ],
  na.rm = TRUE
)

fragmentation_metrics <- fragmentation_metrics |>
  mutate(
    relatively_fragmented =
      total_high_habitat_ha > 0 &
      patch_density_100ha > patch_density_median &
      largest_patch_share < largest_patch_share_median
  )

# 13. Small-patch sensitivity check
calculate_fragmentation <- function(data, min_patch_ha) {
  
  data |>
    filter(
      patch_area_ha >= min_patch_ha
    ) |>
    group_by(psp_name) |>
    summarise(
      patch_count = n_distinct(patch_id),
      habitat_area_ha = sum(patch_area_ha),
      largest_patch_ha = max(patch_area_ha),
      .groups = "drop"
    ) |>
    left_join(
      precinct_area |>
        select(
          psp_name,
          precinct_area_ha
        ),
      by = "psp_name"
    ) |>
    mutate(
      patch_density_100ha =
        patch_count / precinct_area_ha * 100,
      
      largest_patch_share =
        largest_patch_ha / habitat_area_ha * 100,
      
      min_patch_ha = min_patch_ha
    )
}

fragmentation_all <- calculate_fragmentation(
  patch_area_by_precinct,
  0
)

fragmentation_1ha <- calculate_fragmentation(
  patch_area_by_precinct,
  1
)

fragmentation_5ha <- calculate_fragmentation(
  patch_area_by_precinct,
  5
)

fragmentation_sensitivity <- bind_rows(
  fragmentation_all,
  fragmentation_1ha,
  fragmentation_5ha
)

fragmentation_sensitivity |>
  group_by(min_patch_ha) |>
  summarise(
    precincts_with_habitat = n(),
    median_patch_count = median(patch_count),
    median_patch_density = median(patch_density_100ha),
    median_largest_patch_share = median(largest_patch_share),
    .groups = "drop"
  )

# 14. Classify fragmentation across sensitivity scenarios
fragmentation_sensitivity_classified <-
  fragmentation_sensitivity |>
  group_by(min_patch_ha) |>
  mutate(
    scenario_density_median =
      median(
        patch_density_100ha,
        na.rm = TRUE
      ),
    
    scenario_largest_patch_median =
      median(
        largest_patch_share,
        na.rm = TRUE
      ),
    
    fragmented_scenario =
      patch_density_100ha >
      scenario_density_median &
      largest_patch_share <
      scenario_largest_patch_median
  ) |>
  ungroup()

# 15. Identify robust fragmentation
robust_fragmentation <- fragmentation_sensitivity_classified |>
  group_by(psp_name) |>
  summarise(
    fragmented_scenarios =
      sum(
        fragmented_scenario,
        na.rm = TRUE
      ),
    
    robust_fragmented =
      fragmented_scenarios >= 2,
    
    .groups = "drop"
  )

# Add all precincts
robust_fragmentation <- precinct_area |>
  select(
    psp_name,
    lga
  ) |>
  left_join(
    robust_fragmentation,
    by = "psp_name"
  ) |>
  mutate(
    fragmented_scenarios =
      replace_na(
        fragmented_scenarios,
        0L
      ),
    
    robust_fragmented =
      replace_na(
        robust_fragmented,
        FALSE
      )
  )

# Check robust fragmentation
robust_fragmentation |>
  count(
    fragmented_scenarios
  )

robust_fragmentation |>
  count(
    robust_fragmented
  )

# 16. Join baseline screening results
priority_analysis <- read_csv(
  "data/processed/priority_analysis.csv"
)

fragmentation_results <- fragmentation_metrics |>
  left_join(
    robust_fragmentation |>
      select(
        psp_name,
        fragmented_scenarios,
        robust_fragmented
      ),
    by = "psp_name"
  ) |>
  left_join(
    priority_analysis |>
      select(
        psp_name,
        growth_ha,
        urban_share_change,
        high_development,
        high_habitat_percent,
        focal_species_present,
        bcs_percent,
        priority_area
      ),
    by = "psp_name"
  )

# 17. Development-fragmentation overlap
fragmentation_results <- fragmentation_results |>
  mutate(
    development_fragmentation_overlap =
      high_development &
      robust_fragmented
  )

fragmentation_results |>
  count(
    development_fragmentation_overlap
  )

# 18. Compare with baseline priorities
priority_fragmentation_summary <- fragmentation_results |>
  summarise(
    total_precincts = n(),
    
    fragmented_precincts =
      sum(
        robust_fragmented,
        na.rm = TRUE
      ),
    
    development_fragmentation_overlap =
      sum(
        development_fragmentation_overlap,
        na.rm = TRUE
      ),
    
    baseline_priority_precincts =
      sum(
        priority_area,
        na.rm = TRUE
      ),
    
    priority_and_fragmented =
      sum(
        priority_area &
          robust_fragmented,
        na.rm = TRUE
      )
  )

priority_fragmentation_summary

# 19. Compare new and baseline concerns
fragmentation_priority_comparison <- fragmentation_results |>
  filter(
    development_fragmentation_overlap
  ) |>
  mutate(
    baseline_status =
      if_else(
        priority_area,
        "Also baseline priority",
        "New fragmentation concern"
      )
  )

fragmentation_priority_comparison |>
  count(
    baseline_status
  )

fragmentation_priority_comparison |>
  select(
    psp_name,
    lga,
    patch_density_100ha,
    largest_patch_share,
    growth_ha,
    urban_share_change,
    priority_area
  ) |>
  arrange(
    desc(patch_density_100ha)
  ) |>
  print(
    n = Inf,
    width = Inf
  )

# 20. Save results
write_csv(
  fragmentation_results,
  "data/processed/fragmentation_results.csv"
)

# 21. Prepare fragmentation map
fragmentation_map <- msa_study |>
  left_join(
    fragmentation_results |>
      select(
        psp_name,
        robust_fragmented,
        development_fragmentation_overlap
      ),
    by = "psp_name"
  ) |>
  mutate(
    fragmentation_group = case_when(
      development_fragmentation_overlap ~
        "High development + robust fragmentation",
      
      robust_fragmented ~
        "Robustly fragmented habitat",
      
      TRUE ~
        "Other study precinct"
    )
  )

# 22. Plot fragmentation results
fragmentation_plot <- ggplot() +
  geom_sf(
    data = fragmentation_map,
    aes(fill = fragmentation_group),
    colour = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = study_lga_habitat_crs,
    fill = NA,
    colour = "black",
    linewidth = 0.5
  ) +
  labs(
    title = "Robust habitat fragmentation and development pressure",
    subtitle =
      "Fragmentation is compared across all patches, ≥1 ha patches and ≥5 ha patches",
    fill = NULL,
    caption =
      "Robust fragmentation requires agreement across at least two sensitivity scenarios."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      face = "bold"
    )
  )

print(fragmentation_plot)

# 23. Identify major habitat patches
major_patches <- patch_area_by_precinct |>
  filter(
    patch_area_ha >= 5
  )

major_patch_summary <- major_patches |>
  group_by(psp_name) |>
  summarise(
    major_patch_count = n_distinct(patch_id),
    major_habitat_area_ha = sum(patch_area_ha, na.rm = TRUE),
    largest_major_patch_ha = max(patch_area_ha, na.rm = TRUE),
    .groups = "drop"
  )


# 24. Add precincts without major patches
major_patch_summary <- precinct_area |>
  select(
    psp_name,
    lga
  ) |>
  left_join(
    major_patch_summary,
    by = "psp_name"
  ) |>
  mutate(
    major_patch_count =
      replace_na(major_patch_count, 0),
    
    major_habitat_area_ha =
      replace_na(major_habitat_area_ha, 0),
    
    largest_major_patch_ha =
      replace_na(largest_major_patch_ha, 0)
  )

# 25. Create patch polygons
major_patch_ids <- major_patches |>
  distinct(patch_id) |>
  pull(patch_id)

major_patch_raster <- habitat_patches

major_patch_raster[
  !major_patch_raster %in% major_patch_ids
] <- NA

major_patch_polygons <- as.polygons(
  major_patch_raster,
  dissolve = TRUE,
  na.rm = TRUE
)

major_patch_sf <- st_as_sf(
  major_patch_polygons
)

names(major_patch_sf)[1] <- "patch_id"

# 26. Calculate major-patch areas
major_patch_sf <- major_patch_sf |>
  mutate(
    patch_area_ha =
      as.numeric(st_area(geometry)) / 10000
  ) |>
  filter(
    patch_area_ha >= 5
  )

# 27. Nearest distance between major patches
patch_centroids <- st_centroid(
  major_patch_sf
)

patch_distance_matrix <- st_distance(
  patch_centroids
)

# Convert distance matrix to numeric metres
patch_distance_matrix <- matrix(
  as.numeric(patch_distance_matrix),
  nrow = nrow(patch_centroids),
  ncol = nrow(patch_centroids)
)

# Ignore distance from each patch to itself
diag(patch_distance_matrix) <- Inf

patch_centroids$nearest_patch_distance_m <- apply(
  patch_distance_matrix,
  1,
  min,
  na.rm = TRUE
)

# Check that distances actually vary
summary(
  patch_centroids$nearest_patch_distance_m
)

length(
  unique(
    round(
      patch_centroids$nearest_patch_distance_m,
      1
    )
  )
)

# 28. Assign major patches to precincts
major_patch_precinct <- st_join(
  patch_centroids,
  msa_study |>
    select(
      psp_name
    ),
  join = st_within,
  left = TRUE
)

major_patch_precinct_summary <- major_patch_precinct |>
  st_drop_geometry() |>
  filter(
    !is.na(psp_name)
  ) |>
  group_by(psp_name) |>
  summarise(
    major_patch_count_spatial = n(),
    
    mean_nearest_patch_distance_m =
      mean(
        nearest_patch_distance_m,
        na.rm = TRUE
      ),
    
    min_nearest_patch_distance_m =
      min(
        nearest_patch_distance_m,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

# 29. Combine connectivity metrics
connectivity_metrics <- major_patch_summary |>
  left_join(
    major_patch_precinct_summary,
    by = "psp_name"
  ) |>
  mutate(
    major_patch_count_spatial =
      replace_na(
        major_patch_count_spatial,
        0
      ),
    
    mean_nearest_patch_distance_m =
      replace_na(
        mean_nearest_patch_distance_m,
        NA_real_
      ),
    
    min_nearest_patch_distance_m =
      replace_na(
        min_nearest_patch_distance_m,
        NA_real_
      )
  )

# 30. Join fragmentation and development results
connectivity_results <- fragmentation_results |>
  left_join(
    connectivity_metrics |>
      select(
        psp_name,
        major_patch_count,
        major_habitat_area_ha,
        largest_major_patch_ha,
        mean_nearest_patch_distance_m,
        min_nearest_patch_distance_m
      ),
    by = "psp_name"
  )

# 31. Connectivity thresholds
major_patch_count_median <- median(
  connectivity_results$major_patch_count[
    connectivity_results$major_patch_count > 0
  ],
  na.rm = TRUE
)

nearest_distance_median <- median(
  connectivity_results$mean_nearest_patch_distance_m,
  na.rm = TRUE
)

major_habitat_area_median <- median(
  connectivity_results$major_habitat_area_ha[
    connectivity_results$major_habitat_area_ha > 0
  ],
  na.rm = TRUE
)

# 32. Identify connectivity importance
connectivity_results <- connectivity_results |>
  mutate(
    connectivity_importance =
      major_patch_count >= major_patch_count_median &
      (
        major_habitat_area_ha >= major_habitat_area_median |
          mean_nearest_patch_distance_m <= nearest_distance_median
      )
  )

# 33. Identify connectivity priority
connectivity_results <- connectivity_results |>
  mutate(
    connectivity_priority =
      connectivity_importance &
      robust_fragmented &
      high_development
  )

# 34. Summarise connectivity results
connectivity_summary <- connectivity_results |>
  summarise(
    total_precincts = n(),
    
    precincts_with_major_patches =
      sum(
        major_patch_count > 0,
        na.rm = TRUE
      ),
    
    connectivity_important =
      sum(
        connectivity_importance,
        na.rm = TRUE
      ),
    
    robust_fragmented =
      sum(
        robust_fragmented,
        na.rm = TRUE
      ),
    
    high_development =
      sum(
        high_development,
        na.rm = TRUE
      ),
    
    connectivity_priority =
      sum(
        connectivity_priority,
        na.rm = TRUE
      )
  )

connectivity_summary |>
  print(width = Inf)

# 35. Inspect connectivity-priority precincts
connectivity_priority_table <- connectivity_results |>
  filter(
    connectivity_priority
  ) |>
  select(
    psp_name,
    lga,
    major_patch_count,
    major_habitat_area_ha,
    mean_nearest_patch_distance_m,
    patch_density_100ha,
    largest_patch_share,
    robust_fragmented,
    growth_ha,
    urban_share_change,
    high_development,
    priority_area
  ) |>
  arrange(
    mean_nearest_patch_distance_m
  )

connectivity_priority_table |>
  print(
    n = Inf,
    width = Inf
  )

# 36. Compare with baseline screening
connectivity_baseline_comparison <- connectivity_results |>
  filter(
    connectivity_priority
  ) |>
  mutate(
    baseline_status =
      if_else(
        priority_area,
        "Also baseline priority",
        "New connectivity concern"
      )
  )

connectivity_baseline_comparison |>
  count(
    baseline_status
  )

# 37. Save connectivity results
write_csv(
  connectivity_results,
  "data/processed/connectivity_results.csv"
)

# 38. Prepare connectivity map
connectivity_map <- msa_study |>
  left_join(
    connectivity_results |>
      select(
        psp_name,
        connectivity_importance,
        connectivity_priority
      ),
    by = "psp_name"
  ) |>
  mutate(
    connectivity_group = case_when(
      connectivity_priority ~
        "Connectivity priority",
      
      connectivity_importance ~
        "Connectivity importance",
      
      TRUE ~
        "Other study precinct"
    )
  )

# 39. Plot connectivity results
connectivity_plot <- ggplot() +
  geom_sf(
    data = connectivity_map,
    aes(fill = connectivity_group),
    colour = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = major_patch_sf,
    fill = NA,
    colour = "grey30",
    linewidth = 0.25
  ) +
  geom_sf(
    data = study_lga_habitat_crs,
    fill = NA,
    colour = "black",
    linewidth = 0.5
  ) +
  labs(
    title = "Potential habitat connectivity priorities",
    subtitle =
      "Major habitat patches are combined with fragmentation and development pressure",
    fill = NULL,
    caption =
      "Connectivity priorities indicate potential areas for further corridor assessment, not confirmed fauna movement routes."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      face = "bold"
    )
  )

print(connectivity_plot)