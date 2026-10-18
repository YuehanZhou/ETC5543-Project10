library(tidyverse)

# 1. Read your species-connectivity results
species_connectivity <- read_csv(
  "data/processed/species_connectivity_results.csv"
)

# 2. Read processed ESO / protection-gap results
overlay_results <- read_csv(
  "data/processed/overlay_results.csv"
)

# 3. Combine everything at precinct level
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
        no_eso_gap,
        low_eso_gap
      ),
    by = "psp_name"
  )

final_integrated_results <- final_integrated_results |>
  mutate(
    
    # Connectivity priority + focal species evidence
    connectivity_and_species =
      connectivity_priority &
      focal_species_present,
    
    # Connectivity priority with neither BCS nor ESO coverage
    connectivity_protection_gap =
      connectivity_priority &
      bcs_percent == 0 &
      !has_eso,
    
    # Very strong integrated evidence:
    # connectivity priority + species record + no BCS/ESO
    connectivity_species_protection_gap =
      connectivity_priority &
      focal_species_present &
      bcs_percent == 0 &
      !has_eso
  )

final_summary <- final_integrated_results |>
  summarise(
    total_precincts = n(),
    
    connectivity_priorities =
      sum(connectivity_priority, na.rm = TRUE),
    
    connectivity_with_species =
      sum(connectivity_and_species, na.rm = TRUE),
    
    connectivity_without_bcs_eso =
      sum(connectivity_protection_gap, na.rm = TRUE),
    
    connectivity_species_without_bcs_eso =
      sum(
        connectivity_species_protection_gap,
        na.rm = TRUE
      )
  )

final_summary


final_integrated_results |>
  filter(
    connectivity_priority
  ) |>
  select(
    psp_name,
    lga,
    golden_sun_moth_records,
    growling_grass_frog_records,
    focal_species_present,
    high_habitat_percent,
    robust_fragmented,
    connectivity_importance,
    connectivity_priority,
    bcs_percent,
    eso_percent,
    protection_type,
    connectivity_and_species,
    connectivity_protection_gap,
    connectivity_species_protection_gap
  ) |>
  arrange(
    desc(connectivity_species_protection_gap),
    desc(connectivity_and_species),
    desc(connectivity_protection_gap)
  ) |>
  print(
    n = Inf,
    width = Inf
  )


write_csv(
  final_integrated_results,
  "data/processed/final_integrated_results.csv"
)

write_csv(
  final_summary,
  "data/processed/final_integrated_summary.csv"
)



final_integrated_results |>
  filter(
    connectivity_species_protection_gap
  ) |>
  select(
    psp_name,
    lga,
    golden_sun_moth_records,
    growling_grass_frog_records,
    high_habitat_percent,
    bcs_percent,
    eso_percent,
    protection_type,
    robust_fragmented,
    connectivity_importance,
    connectivity_priority
  ) |>
  print(
    n = Inf,
    width = Inf
  )


final_integrated_results |>
  filter(
    connectivity_protection_gap
  ) |>
  select(
    psp_name,
    lga,
    golden_sun_moth_records,
    growling_grass_frog_records,
    focal_species_present,
    high_habitat_percent,
    bcs_percent,
    eso_percent,
    protection_type
  ) |>
  arrange(
    desc(focal_species_present),
    desc(high_habitat_percent)
  ) |>
  print(
    n = Inf,
    width = Inf
  )

# Final integrated priority table
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
    
    protection_gap =
      if_else(
        bcs_percent == 0 &
          eso_percent == 0,
        "Yes",
        "No"
      ),
    
    strong_integrated_priority =
      if_else(
        connectivity_priority &
          focal_species_present &
          bcs_percent == 0 &
          eso_percent == 0,
        "Yes",
        "No"
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
    protection_gap,
    strong_integrated_priority
  ) |>
  arrange(
    desc(strong_integrated_priority),
    desc(protection_gap),
    desc(focal_species_evidence),
    desc(high_habitat_percent)
  )

final_priority_table |>
  print(
    n = Inf,
    width = Inf
  )

write_csv(
  final_priority_table,
  "data/processed/final_priority_table.csv"
)


# Create simple integrated evidence categories
final_priority_table <- final_priority_table |>
  mutate(
    evidence_category = case_when(
      
      focal_species_evidence == "Yes" &
        protection_gap == "Yes" ~
        "Species evidence + protection gap",
      
      focal_species_evidence == "No" &
        protection_gap == "Yes" ~
        "Protection gap",
      
      focal_species_evidence == "Yes" &
        protection_gap == "No" ~
        "Species evidence + some protection",
      
      TRUE ~
        "Other connectivity priority"
    )
  )

final_priority_table |>
  count(
    evidence_category,
    sort = TRUE
  )

final_priority_table |>
  arrange(
    desc(strong_integrated_priority),
    desc(protection_gap),
    desc(focal_species_evidence)
  ) |>
  select(
    psp_name,
    high_habitat_percent,
    focal_species_evidence,
    bcs_percent,
    eso_percent,
    protection_gap,
    strong_integrated_priority,
    evidence_category
  ) |>
  print(
    n = Inf,
    width = Inf
  )

write_csv(
  final_priority_table,
  "data/processed/final_priority_table.csv"
)