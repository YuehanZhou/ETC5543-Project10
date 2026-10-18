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

# Priority analysis
priority_analysis <- read_csv(
  "data/processed/priority_analysis.csv",
  show_col_types = FALSE
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
# Keep the analytical study precincts
msa_study_attributes <- msa |>
  filter(
    psp_name %in% priority_analysis$psp_name
  )

# Join attributes to PSP geometry
msa_spatial <- psp_boundary |>
  filter(
    PSP_NAME %in% priority_analysis$psp_name
  ) |>
  left_join(
    msa_study_attributes,
    by = c("PSP_NAME" = "psp_name")
  ) |>
  rename(
    psp_name = PSP_NAME
  ) |>
  filter(
    !st_is_empty(geometry)
  )

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

stopifnot(
  n_distinct(msa_study$psp_name) == 101,
  !anyDuplicated(msa_study$psp_name),
  setequal(
    msa_study$psp_name,
    priority_analysis$psp_name
  )
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
habitat_patches <- patches(
  high_habitat,
  directions = 8
)

names(habitat_patches) <- "patch_id"

# Check number of habitat patches across the study area
patch_frequency <- freq(
  habitat_patches
)

# 5. Calculate raster cell area
cell_area_ha <- prod(
  res(habitat_patches)
) / 10000

patch_total_area <- patch_frequency |>
  transmute(
    patch_id = value,
    whole_patch_area_ha =
      count * cell_area_ha
  )

# 6. Extract habitat patches within each PSP/MSA precinct
patch_extract <- terra::extract(
  habitat_patches,
  vect(msa_study),
  exact = TRUE,
  cells = TRUE
)

patch_extract <- patch_extract |>
  mutate(
    psp_name = msa_study$psp_name[ID]
  ) |>
  filter(
    !is.na(patch_id),
    !is.na(fraction),
    fraction > 0
  )

# 7. Calculate patch-level area within each precinct
patch_area_by_precinct <- patch_extract |>
  group_by(
    psp_name,
    patch_id
  ) |>
  summarise(
    habitat_cells = sum(fraction),
    patch_area_ha = sum(fraction) * cell_area_ha,
    .groups = "drop"
  )

patch_area_by_precinct <- patch_area_by_precinct |>
  left_join(
    patch_total_area,
    by = "patch_id"
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
      if_else(
        total_high_habitat_ha > 0,
        largest_patch_share,
        NA_real_
      ),
    
    patch_density_100ha =
      if_else(
        precinct_area_ha > 0,
        habitat_patch_count / precinct_area_ha * 100,
        NA_real_
      )
  )

# 11. Inspect fragmentation results
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

# 12. Small-patch sensitivity check
calculate_fragmentation <- function(
    data,
    min_patch_ha,
    precinct_area
) {
  
  eligible_patches <- data |>
    filter(
      whole_patch_area_ha >= min_patch_ha
    ) |>
    group_by(psp_name) |>
    summarise(
      patch_count = n_distinct(patch_id),
      habitat_area_ha =
        sum(patch_area_ha, na.rm = TRUE),
      largest_patch_ha =
        max(patch_area_ha, na.rm = TRUE),
      .groups = "drop"
    )
  
  precinct_area |>
    select(
      psp_name,
      precinct_area_ha
    ) |>
    left_join(
      eligible_patches,
      by = "psp_name"
    ) |>
    mutate(
      patch_count =
        replace_na(patch_count, 0L),
      
      habitat_area_ha =
        replace_na(habitat_area_ha, 0),
      
      largest_patch_ha =
        replace_na(largest_patch_ha, 0),
      
      eligible_scenario =
        habitat_area_ha > 0,
      
      patch_density_100ha =
        patch_count /
        precinct_area_ha * 100,
      
      largest_patch_share =
        if_else(
          eligible_scenario,
          largest_patch_ha /
            habitat_area_ha * 100,
          NA_real_
        ),
      
      min_patch_ha = min_patch_ha
    )
}

fragmentation_all <- calculate_fragmentation(
  patch_area_by_precinct,
  0,
  precinct_area
)

fragmentation_1ha <- calculate_fragmentation(
  patch_area_by_precinct,
  1,
  precinct_area
)

fragmentation_5ha <- calculate_fragmentation(
  patch_area_by_precinct,
  5,
  precinct_area
)

fragmentation_sensitivity <- bind_rows(
  fragmentation_all,
  fragmentation_1ha,
  fragmentation_5ha
)

fragmentation_sensitivity |>
  group_by(min_patch_ha) |>
  summarise(
    eligible_precincts =
      sum(eligible_scenario),
    
    median_patch_count =
      median(
        patch_count[eligible_scenario],
        na.rm = TRUE
      ),
    
    median_patch_density =
      median(
        patch_density_100ha[eligible_scenario],
        na.rm = TRUE
      ),
    
    median_largest_patch_share =
      median(
        largest_patch_share[eligible_scenario],
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

# 13. Classify fragmentation across sensitivity scenarios
fragmentation_sensitivity_classified <-
  fragmentation_sensitivity |>
  group_by(min_patch_ha) |>
  mutate(
    scenario_density_median =
      median(
        patch_density_100ha[eligible_scenario],
        na.rm = TRUE
      ),
    
    scenario_largest_patch_median =
      median(
        largest_patch_share[eligible_scenario],
        na.rm = TRUE
      ),
    
    fragmented_scenario =
      eligible_scenario &
      patch_density_100ha >
      scenario_density_median &
      largest_patch_share <
      scenario_largest_patch_median
  ) |>
  ungroup()

# 14. Identify robust fragmentation
robust_fragmentation <-
  fragmentation_sensitivity_classified |>
  group_by(psp_name) |>
  summarise(
    eligible_scenarios =
      sum(
        eligible_scenario,
        na.rm = TRUE
      ),
    
    fragmented_scenarios =
      sum(
        fragmented_scenario,
        na.rm = TRUE
      ),
    
    robust_fragmented =
      eligible_scenarios >= 2 &
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
    eligible_scenarios =
      replace_na(
        eligible_scenarios,
        0L
      ),
    
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
    eligible_scenarios,
    fragmented_scenarios
  )

robust_fragmentation |>
  count(
    robust_fragmented
  )

# 15. Join baseline screening results
fragmentation_results <- fragmentation_metrics |>
  left_join(
    robust_fragmentation |>
      select(
        psp_name,
        eligible_scenarios,
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

# 16. Development-fragmentation overlap
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

# 17. Compare with baseline priorities
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

# 18. Compare new and baseline concerns
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

# 19. Save results
write_csv(
  fragmentation_results,
  "data/processed/fragmentation_results.csv"
)

# 20. Prepare fragmentation map
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

# 21. Plot fragmentation results
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
      "Robust fragmentation requires classification as fragmented in at least two eligible sensitivity scenarios."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      face = "bold"
    )
  )

print(fragmentation_plot)

# 22. Identify major habitat patches
major_patches <- patch_area_by_precinct |>
  filter(
    whole_patch_area_ha >= 5
  )

major_patch_summary <- major_patches |>
  group_by(psp_name) |>
  summarise(
    major_patch_count = n_distinct(patch_id),
    major_habitat_area_ha = sum(patch_area_ha, na.rm = TRUE),
    largest_major_patch_ha = max(patch_area_ha, na.rm = TRUE),
    .groups = "drop"
  )


# 23. Add precincts without major patches
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

# 24. Create patch polygons
major_patch_ids <- patch_total_area |>
  filter(
    whole_patch_area_ha >= 5
  ) |>
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

# 25. Add whole-patch area information
major_patch_sf <- major_patch_sf |>
  left_join(
    patch_total_area,
    by = "patch_id"
  )

# 26. Nearest distance between major patches
patch_distance_matrix <- st_distance(
  major_patch_sf
)

patch_distance_matrix <- matrix(
  as.numeric(patch_distance_matrix),
  nrow = nrow(major_patch_sf),
  ncol = nrow(major_patch_sf)
)

diag(patch_distance_matrix) <- Inf

major_patch_sf$nearest_patch_distance_m <- apply(
  patch_distance_matrix,
  1,
  min
)

patch_distance_table <- major_patch_sf |>
  st_drop_geometry() |>
  select(
    patch_id,
    nearest_patch_distance_m
  )

major_patch_precinct <- major_patches |>
  left_join(
    patch_distance_table,
    by = "patch_id"
  )

# 27. Summarise major-patch distances by precinct
major_patch_precinct_summary <-
  major_patch_precinct |>
  group_by(psp_name) |>
  summarise(
    major_patch_count_spatial =
      n_distinct(patch_id),
    
    mean_nearest_patch_distance_m =
      if (
        any(
          is.finite(
            nearest_patch_distance_m
          )
        )
      ) {
        
        valid <-
          is.finite(
            nearest_patch_distance_m
          )
        
        weighted.mean(
          nearest_patch_distance_m[valid],
          w = patch_area_ha[valid]
        )
        
      } else {
        NA_real_
      },
    
    min_nearest_patch_distance_m =
      if (
        any(
          is.finite(
            nearest_patch_distance_m
          )
        )
      ) {
        
        min(
          nearest_patch_distance_m[
            is.finite(
              nearest_patch_distance_m
            )
          ]
        )
        
      } else {
        NA_real_
      },
    
    .groups = "drop"
  )

# 28. Combine connectivity metrics
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

# 29. Join fragmentation and development results
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

# 30. Connectivity thresholds
safe_median <- function(x) {
  x <- x[
    is.finite(x)
  ]
  
  if (length(x) == 0) {
    NA_real_
  } else {
    median(x)
  }
}

major_patch_count_median <- safe_median(
  connectivity_results$major_patch_count[
    connectivity_results$major_patch_count > 0
  ]
)

nearest_distance_median <- safe_median(
  connectivity_results$mean_nearest_patch_distance_m
)

major_habitat_area_median <- safe_median(
  connectivity_results$major_habitat_area_ha[
    connectivity_results$major_habitat_area_ha > 0
  ]
)

# 31. Identify connectivity importance
connectivity_results <- connectivity_results |>
  mutate(
    connectivity_importance =
      major_patch_count > 0 &
      coalesce(
        major_patch_count >=
          major_patch_count_median,
        FALSE
      ) &
      (
        coalesce(
          major_habitat_area_ha >=
            major_habitat_area_median,
          FALSE
        ) |
          coalesce(
            mean_nearest_patch_distance_m <=
              nearest_distance_median,
            FALSE
          )
      )
  )

# 32. Identify connectivity priority
connectivity_results <- connectivity_results |>
  mutate(
    connectivity_priority =
      connectivity_importance &
      robust_fragmented &
      high_development
  )

# 33. Summarise connectivity results
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

# 34. Inspect connectivity-priority precincts
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

# 35. Compare with baseline screening
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

# 36. Connectivity threshold sensitivity

connectivity_patch_thresholds <- c(1, 5, 10)

calculate_connectivity_scenario <- function(
    min_major_patch_ha
) {
  
  # Identify patches meeting the scenario threshold
  eligible_patch_ids <- patch_total_area |>
    filter(
      whole_patch_area_ha >= min_major_patch_ha
    ) |>
    pull(patch_id)
  
  # Summarise eligible habitat patches within each precinct
  scenario_patch_summary <-
    patch_area_by_precinct |>
    filter(
      patch_id %in% eligible_patch_ids
    ) |>
    group_by(psp_name) |>
    summarise(
      major_patch_count =
        n_distinct(patch_id),
      
      major_habitat_area_ha =
        sum(
          patch_area_ha,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    )
  
  # Create threshold-specific patch raster
  scenario_patch_raster <- habitat_patches
  
  scenario_patch_raster[
    !scenario_patch_raster %in%
      eligible_patch_ids
  ] <- NA
  
  # Convert eligible patches to polygons
  scenario_patch_polygons <- as.polygons(
    scenario_patch_raster,
    dissolve = TRUE,
    na.rm = TRUE
  )
  
  scenario_patch_sf <- st_as_sf(
    scenario_patch_polygons
  )
  
  names(scenario_patch_sf)[1] <- "patch_id"
  
  # Calculate nearest-patch distance separately
  # for this threshold
  if (nrow(scenario_patch_sf) >= 2) {
    
    scenario_distance_matrix <- st_distance(
      scenario_patch_sf
    )
    
    scenario_distance_matrix <- matrix(
      as.numeric(
        scenario_distance_matrix
      ),
      nrow = nrow(
        scenario_patch_sf
      ),
      ncol = nrow(
        scenario_patch_sf
      )
    )
    
    diag(
      scenario_distance_matrix
    ) <- Inf
    
    scenario_patch_sf$nearest_patch_distance_m <-
      apply(
        scenario_distance_matrix,
        1,
        min
      )
    
  } else {
    
    scenario_patch_sf$nearest_patch_distance_m <-
      NA_real_
  }
  
  # Create patch-level distance table
  scenario_patch_distance <-
    scenario_patch_sf |>
    st_drop_geometry() |>
    select(
      patch_id,
      nearest_patch_distance_m
    )
  
  # Summarise nearest-patch distance by precinct
  scenario_distance_summary <-
    patch_area_by_precinct |>
    filter(
      patch_id %in%
        eligible_patch_ids
    ) |>
    left_join(
      scenario_patch_distance,
      by = "patch_id"
    ) |>
    group_by(psp_name) |>
    summarise(
      mean_nearest_patch_distance_m =
        if (
          any(
            is.finite(
              nearest_patch_distance_m
            )
          )
        ) {
          
          valid <- is.finite(
            nearest_patch_distance_m
          )
          
          weighted.mean(
            nearest_patch_distance_m[valid],
            w = patch_area_ha[valid]
          )
          
        } else {
          NA_real_
        },
      
      .groups = "drop"
    )
  
  # Add all analytical precincts
  scenario <- fragmentation_results |>
    select(
      psp_name,
      robust_fragmented,
      high_development
    ) |>
    left_join(
      scenario_patch_summary,
      by = "psp_name"
    ) |>
    left_join(
      scenario_distance_summary,
      by = "psp_name"
    ) |>
    mutate(
      major_patch_count =
        replace_na(
          major_patch_count,
          0L
        ),
      
      major_habitat_area_ha =
        replace_na(
          major_habitat_area_ha,
          0
        )
    )
  
  # Scenario-specific thresholds
  count_median <- safe_median(
    scenario$major_patch_count[
      scenario$major_patch_count > 0
    ]
  )
  
  area_median <- safe_median(
    scenario$major_habitat_area_ha[
      scenario$major_habitat_area_ha > 0
    ]
  )
  
  distance_median <- safe_median(
    scenario$mean_nearest_patch_distance_m
  )
  
  # Identify connectivity importance and priority
  scenario |>
    mutate(
      connectivity_importance =
        major_patch_count > 0 &
        coalesce(
          major_patch_count >=
            count_median,
          FALSE
        ) &
        (
          coalesce(
            major_habitat_area_ha >=
              area_median,
            FALSE
          ) |
            coalesce(
              mean_nearest_patch_distance_m <=
                distance_median,
              FALSE
            )
        ),
      
      connectivity_priority =
        connectivity_importance &
        robust_fragmented &
        high_development,
      
      major_patch_threshold_ha =
        min_major_patch_ha
    ) |>
    select(
      psp_name,
      major_patch_threshold_ha,
      major_patch_count,
      major_habitat_area_ha,
      mean_nearest_patch_distance_m,
      connectivity_importance,
      connectivity_priority
    )
}

connectivity_threshold_sensitivity <-
  map_dfr(
    connectivity_patch_thresholds,
    calculate_connectivity_scenario
  )

# Summarise sensitivity results
connectivity_threshold_sensitivity |>
  group_by(
    major_patch_threshold_ha
  ) |>
  summarise(
    precincts_with_major_patches =
      sum(
        major_patch_count > 0
      ),
    
    connectivity_important =
      sum(
        connectivity_importance
      ),
    
    connectivity_priority =
      sum(
        connectivity_priority
      ),
    
    .groups = "drop"
  )

# Assess consistency across thresholds
connectivity_sensitivity_summary <-
  connectivity_threshold_sensitivity |>
  group_by(psp_name) |>
  summarise(
    priority_scenarios =
      sum(
        connectivity_priority,
        na.rm = TRUE
      ),
    
    stable_connectivity_priority =
      priority_scenarios >= 2,
    
    .groups = "drop"
  )

connectivity_sensitivity_summary |>
  count(
    priority_scenarios
  )

connectivity_sensitivity_summary |>
  count(
    stable_connectivity_priority
  )

# 37. Save connectivity results
write_csv(
  connectivity_results,
  "data/processed/connectivity_results.csv"
)

write_csv(
  connectivity_threshold_sensitivity,
  "data/processed/connectivity_threshold_sensitivity.csv"
)

write_csv(
  connectivity_sensitivity_summary,
  "data/processed/connectivity_sensitivity_summary.csv"
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