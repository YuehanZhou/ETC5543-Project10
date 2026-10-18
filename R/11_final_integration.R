library(tidyverse)
library(sf)

# 1. Read processed results
species_connectivity <- read_csv(
  "data/processed/species_connectivity_results.csv"
)

overlay_results <- read_csv(
  "data/processed/overlay_results.csv"
)

# 2. Read spatial data for the final map
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

# 3. Prepare study area
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
  filter(
    !st_is_empty(geometry)
  )

msa_vicgrid <- st_transform(
  msa_spatial,
  3111
)

study_lga_vicgrid <- st_transform(
  study_lga,
  3111
)

study_area <- st_union(
  study_lga_vicgrid
)

msa_study <- suppressWarnings(
  st_intersection(
    msa_vicgrid,
    study_area
  )
) |>
  group_by(
    psp_name
  ) |>
  summarise(
    lga = first(lga),
    geometry = st_union(geometry),
    .groups = "drop"
  )

# 4. Combine species, connectivity and protection results
final_integrated_results <- species_connectivity |>
  left_join(
    overlay_results |>
      select(
        psp_name,
        eso_area_ha,
        eso_percent,
        has_eso,
        eso_schedules,
        protection_type,
        protection_gap
      ),
    by = "psp_name"
  ) |>
  mutate(
    bcs_percent = replace_na(bcs_percent, 0),
    eso_percent = replace_na(eso_percent, 0),
    has_eso = replace_na(has_eso, FALSE),
    focal_species_present = replace_na(focal_species_present, FALSE),
    connectivity_priority = replace_na(connectivity_priority, FALSE),
    protection_gap = replace_na(protection_gap, FALSE),
    
    # Connectivity priority + focal species evidence
    connectivity_and_species =
      connectivity_priority &
      focal_species_present,
    
    # Connectivity priority + focal species evidence + no BCS/ESO coverage
    connectivity_species_protection_gap =
      connectivity_priority &
      focal_species_present &
      protection_gap
  )

# 5. Final summary
final_summary <- final_integrated_results |>
  summarise(
    total_precincts = n(),
    
    connectivity_priorities =
      sum(connectivity_priority, na.rm = TRUE),
    
    connectivity_with_species =
      sum(connectivity_and_species, na.rm = TRUE),
    
    connectivity_without_bcs_eso =
      sum(protection_gap, na.rm = TRUE),
    
    connectivity_species_without_bcs_eso =
      sum(
        connectivity_species_protection_gap,
        na.rm = TRUE
      )
  )

final_summary

# 6. Final priority table
final_priority_table <- final_integrated_results |>
  filter(
    connectivity_priority
  ) |>
  mutate(
    focal_species_evidence =
      if_else(
        focal_species_present,
        "Yes",
        "No"
      ),
    
    bcs_coverage =
      if_else(
        bcs_percent > 0,
        "Yes",
        "No"
      ),
    
    eso_coverage =
      if_else(
        eso_percent > 0,
        "Yes",
        "No"
      ),
    
    protection_gap_label =
      if_else(
        protection_gap,
        "Yes",
        "No"
      ),
    
    strong_integrated_priority =
      if_else(
        focal_species_present &
          protection_gap,
        "Yes",
        "No"
      ),
    
    evidence_category = case_when(
      focal_species_present &
        protection_gap ~
        "Species evidence + no BCS/ESO",
      
      !focal_species_present &
        protection_gap ~
        "No mapped BCS/ESO coverage",
      
      focal_species_present &
        !protection_gap ~
        "Species evidence + some protection",
      
      TRUE ~
        "Other connectivity priority"
    )
  ) |>
  select(
    psp_name,
    lga,
    high_habitat_percent,
    golden_sun_moth_records,
    growling_grass_frog_records,
    focal_species_evidence,
    bcs_percent,
    eso_percent,
    bcs_coverage,
    eso_coverage,
    protection_gap_label,
    strong_integrated_priority,
    evidence_category
  ) |>
  arrange(
    desc(strong_integrated_priority),
    desc(protection_gap_label),
    desc(focal_species_evidence),
    desc(high_habitat_percent)
  )

final_priority_table |>
  print(
    n = Inf,
    width = Inf
  )

# 7. Category summary
integrated_category_counts <- final_priority_table |>
  count(
    evidence_category,
    name = "precincts"
  )

integrated_category_counts

# 8. Category plot
integrated_category_plot <- ggplot(
  integrated_category_counts,
  aes(
    x = reorder(
      evidence_category,
      precincts
    ),
    y = precincts
  )
) +
  geom_col() +
  coord_flip() +
  geom_text(
    aes(
      label = precincts
    ),
    hjust = -0.2
  ) +
  labs(
    title =
      "Integrated evidence across connectivity-priority precincts",
    x = NULL,
    y = "Number of precincts",
    caption =
      "14 connectivity-priority precincts in total"
  ) +
  theme_minimal()

print(
  integrated_category_plot
)

ggsave(
  "figures/integrated_priority_categories.png",
  integrated_category_plot,
  width = 9,
  height = 5,
  dpi = 300
)


# 9. Final integrated map
final_priority_map <- msa_study |>
  left_join(
    final_priority_table |>
      select(
        psp_name,
        evidence_category
      ),
    by = "psp_name"
  ) |>
  mutate(
    evidence_category =
      replace_na(
        evidence_category,
        "Not a connectivity priority"
      )
  )

final_integrated_map <- ggplot() +
  geom_sf(
    data = final_priority_map,
    aes(
      fill = evidence_category
    ),
    colour = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = study_lga_vicgrid,
    fill = NA,
    colour = "black",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "Not a connectivity priority" = "grey85",
      "Other connectivity priority" = "#00BA84",
      "No mapped BCS/ESO coverage" = "#F8766D",
      "Species evidence + no BCS/ESO" = "#00A9E0",
      "Species evidence + some protection" = "#D65ADF"
    ),
    breaks = c(
      "Not a connectivity priority",
      "Other connectivity priority",
      "No mapped BCS/ESO coverage",
      "Species evidence + no BCS/ESO",
      "Species evidence + some protection"
    )
  ) +
  labs(
    title =
      "Integrated connectivity and protection priorities",
    subtitle =
      "Connectivity-priority precincts classified by focal-species evidence and planning protection",
    fill = NULL,
    caption =
      "No mapped BCS/ESO = no mapped BCS or ESO coverage. No focal-species record does not imply species absence."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title =
      element_text(
        face = "bold"
      )
  )

print(
  final_integrated_map
)

ggsave(
  "figures/final_integrated_priority_map.png",
  final_integrated_map,
  width = 10,
  height = 7,
  dpi = 300
)


# 10. Save final outputs
write_csv(
  final_integrated_results,
  "data/processed/final_integrated_results.csv"
)

write_csv(
  final_summary,
  "data/processed/final_integrated_summary.csv"
)

write_csv(
  final_priority_table,
  "data/processed/final_priority_table.csv"
)