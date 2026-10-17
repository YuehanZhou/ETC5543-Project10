library(tidyverse)
library(sf)

# 1. Read datasets
planning_overlay <- st_read(
  "data/Planning_Overlay/ll_gda2020/esrishape/whole_of_dataset/victoria/VMPLAN/PLAN_OVERLAY.shp",
  quiet = TRUE
)

msa <- read_csv(
  "data/meeting1/msa_precinct_attributes.csv"
)

psp_boundary <- st_read(
  "data/PSP_Boundary/PSP_Boundary.geojson",
  quiet = TRUE
)

lga_boundary <- st_read(
  "data/LGA_2025_AUST_GDA2020/LGA_2025_AUST_GDA2020.shp",
  quiet = TRUE
)

priority_analysis <- read_csv(
  "data/processed/priority_analysis.csv"
)

connectivity_results <- read_csv(
  "data/processed/connectivity_results.csv"
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

psp_boundary <- psp_boundary |>
  mutate(
    PSP_NAME = case_when(
      PSP_NAME == "Quandong PSP (Council-led)" ~ "Quandong PSP",
      PSP_NAME == "Croskell" ~ "Croskell (Casey Central)",
      TRUE ~ PSP_NAME
    )
  )

msa_spatial <- msa |>
  left_join(
    psp_boundary,
    by = c("psp_name" = "PSP_NAME")
  ) |>
  st_as_sf() |>
  filter(!st_is_empty(geometry))

msa_vicgrid <- st_transform(
  msa_spatial,
  3111
)

study_lga_vicgrid <- st_transform(
  study_lga,
  3111
)

study_area <- study_lga_vicgrid |>
  st_union()

msa_study <- suppressWarnings(
  st_intersection(
    msa_vicgrid,
    study_area
  )
) |>
  group_by(psp_name) |>
  summarise(
    lga = first(lga),
    geometry = st_union(geometry),
    .groups = "drop"
  )

# 3. Filter Environmental Significance Overlays
eso <- planning_overlay |>
  filter(
    SCHEMECODE == "ESO"
  )

eso_vicgrid <- st_transform(
  eso,
  3111
)

eso_study <- suppressWarnings(
  st_intersection(
    eso_vicgrid,
    study_area
  )
)

# 4. Check ESO schedules in study area
eso_schedule_summary <- eso_study |>
  st_drop_geometry() |>
  count(
    ZONE_CODE,
    ZONE_DESC,
    sort = TRUE
  )

eso_schedule_summary

# 5. Calculate precinct area
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
  )

# 6. Create total ESO coverage layer
eso_union <- eso_study |>
  st_make_valid() |>
  st_union() |>
  st_sf()

# 7. Calculate ESO coverage by precinct
eso_precinct_overlap <- suppressWarnings(
  st_intersection(
    msa_study |>
      select(
        psp_name,
        lga
      ),
    eso_union
  )
)

eso_by_precinct <- eso_precinct_overlap |>
  mutate(
    eso_area_ha =
      as.numeric(st_area(geometry)) / 10000
  ) |>
  st_drop_geometry() |>
  group_by(psp_name) |>
  summarise(
    eso_area_ha =
      sum(
        eso_area_ha,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

# Keep ESO schedule information separately
eso_schedule_by_precinct <- suppressWarnings(
  st_intersection(
    msa_study |>
      select(psp_name),
    eso_study |>
      select(
        ZONE_CODE
      )
  )
) |>
  st_drop_geometry() |>
  group_by(psp_name) |>
  summarise(
    eso_schedule_count =
      n_distinct(ZONE_CODE),
    
    eso_schedules =
      paste(
        sort(unique(ZONE_CODE)),
        collapse = ", "
      ),
    
    .groups = "drop"
  )

# 8. Add precincts with no ESO coverage
eso_results <- precinct_area |>
  left_join(
    eso_by_precinct,
    by = "psp_name"
  ) |>
  left_join(
    eso_schedule_by_precinct,
    by = "psp_name"
  ) |>
  mutate(
    eso_area_ha =
      replace_na(
        eso_area_ha,
        0
      ),
    
    eso_schedule_count =
      replace_na(
        eso_schedule_count,
        0L
      ),
    
    eso_schedules =
      replace_na(
        eso_schedules,
        "None"
      ),
    
    eso_percent =
      if_else(
        precinct_area_ha > 0,
        eso_area_ha / precinct_area_ha * 100,
        NA_real_
      ),
    
    has_eso =
      eso_area_ha > 0
  )

# 9. Join baseline and connectivity results
overlay_results <- eso_results |>
  left_join(
    priority_analysis |>
      select(
        psp_name,
        bcs_percent,
        priority_area,
        high_development,
        high_ecological_value
      ),
    by = "psp_name"
  ) |>
  left_join(
    connectivity_results |>
      select(
        psp_name,
        robust_fragmented,
        connectivity_importance,
        connectivity_priority
      ),
    by = "psp_name"
  )

overlay_results <- overlay_results |>
  mutate(
    bcs_percent = replace_na(bcs_percent, 0),
    priority_area = replace_na(priority_area, FALSE),
    high_development = replace_na(high_development, FALSE),
    high_ecological_value = replace_na(high_ecological_value, FALSE),
    robust_fragmented = replace_na(robust_fragmented, FALSE),
    connectivity_importance = replace_na(connectivity_importance, FALSE),
    connectivity_priority = replace_na(connectivity_priority, FALSE)
  )

# 10. Combined protection categories
overlay_results <- overlay_results |>
  mutate(
    protection_type = case_when(
      bcs_percent > 0 & has_eso ~
        "BCS + ESO",
      
      bcs_percent > 0 & !has_eso ~
        "BCS only",
      
      bcs_percent == 0 & has_eso ~
        "ESO only",
      
      TRUE ~
        "Neither"
    )
  )

# 11. Potential planning protection gap
overlay_results <- overlay_results |>
  mutate(
    no_eso_gap =
      connectivity_priority &
      !has_eso,
    
    low_eso_gap =
      connectivity_priority &
      eso_percent < 10
  )

# 12. Summary
overlay_summary <- overlay_results |>
  summarise(
    total_precincts = n(),
    
    precincts_with_eso =
      sum(
        has_eso,
        na.rm = TRUE
      ),
    
    baseline_priority =
      sum(
        priority_area,
        na.rm = TRUE
      ),
    
    connectivity_priority =
      sum(
        connectivity_priority,
        na.rm = TRUE
      ),
    
    connectivity_priority_without_eso =
      sum(
        no_eso_gap,
        na.rm = TRUE
      ),
    
    connectivity_priority_low_eso =
      sum(
        low_eso_gap,
        na.rm = TRUE
      )
  )

overlay_summary

# 13. Protection type summary
overlay_results |>
  count(
    protection_type,
    sort = TRUE
  )

# 14. Connectivity priorities and ESO coverage
connectivity_eso_table <- overlay_results |>
  filter(
    connectivity_priority
  ) |>
  select(
    psp_name,
    lga,
    eso_area_ha,
    eso_percent,
    has_eso,
    eso_schedules,
    bcs_percent,
    protection_type,
    priority_area,
    no_eso_gap,
    low_eso_gap
  ) |>
  arrange(
    eso_percent
  )

connectivity_eso_table |>
  as_tibble() |>
  print(
    n = Inf,
    width = Inf
  )

# 15. Baseline priorities and ESO coverage
baseline_eso_table <- overlay_results |>
  filter(
    priority_area
  ) |>
  select(
    psp_name,
    lga,
    eso_area_ha,
    eso_percent,
    has_eso,
    eso_schedules,
    bcs_percent,
    protection_type,
    connectivity_priority
  ) |>
  arrange(
    eso_percent
  )

baseline_eso_table |>
  as_tibble() |>
  print(n = Inf, width = Inf)

# 16. Save results
write_csv(
  overlay_results,
  "data/processed/overlay_results.csv"
)


# 17. Prepare map
overlay_map <- msa_study |>
  left_join(
    overlay_results |>
      select(
        psp_name,
        connectivity_priority,
        no_eso_gap
      ),
    by = "psp_name"
  ) |>
  mutate(
    overlay_group = case_when(
      no_eso_gap ~
        "Connectivity priority without ESO",
      
      connectivity_priority ~
        "Connectivity priority with ESO",
      
      TRUE ~
        "Other study precinct"
    )
  )

# 18. Plot planning protection gap
overlay_plot <- ggplot() +
  geom_sf(
    data = overlay_map,
    aes(
      fill = overlay_group
    ),
    colour = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = eso_study,
    fill = NA,
    colour = "grey30",
    linewidth = 0.25
  ) +
  geom_sf(
    data = study_lga_vicgrid,
    fill = NA,
    colour = "black",
    linewidth = 0.5
  ) +
  labs(
    title = "Connectivity priorities and ESO coverage",
    subtitle =
      "Potential connectivity priorities are compared with Environmental Significance Overlay coverage",
    fill = NULL,
    caption =
      "No ESO does not imply absence of all environmental protection."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      face = "bold"
    )
  )

print(
  overlay_plot
)