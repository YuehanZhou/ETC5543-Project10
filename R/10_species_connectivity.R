library(tidyverse)

species <- read_csv(
  "data/processed/focal_species_precincts.csv"
)

connectivity <- read_csv(
  "data/processed/connectivity_results.csv"
)

names(species)
names(connectivity)

# 1. Start with all 101 analytical precincts, then add species records
species_connectivity <- connectivity |>
  left_join(
    species |>
      select(
        psp_name,
        golden_sun_moth_records,
        growling_grass_frog_records
      ),
    by = "psp_name"
  ) |>
  mutate(
    golden_sun_moth_records =
      replace_na(golden_sun_moth_records, 0),
    
    growling_grass_frog_records =
      replace_na(growling_grass_frog_records, 0),
    
    golden_sun_moth_present =
      golden_sun_moth_records > 0,
    
    growling_grass_frog_present =
      growling_grass_frog_records > 0,
    
    focal_species_present =
      golden_sun_moth_present |
      growling_grass_frog_present,
    
    species_group = case_when(
      golden_sun_moth_present &
        growling_grass_frog_present ~
        "Both species",
      
      golden_sun_moth_present ~
        "Golden Sun Moth",
      
      growling_grass_frog_present ~
        "Growling Grass Frog",
      
      TRUE ~
        "No focal-species record"
    )
  )
# 2. Quick summary
species_connectivity |>
  summarise(
    total_precincts = n(),
    
    precincts_with_focal_species =
      sum(
        focal_species_present,
        na.rm = TRUE
      ),
    
    connectivity_priority_precincts =
      sum(
        connectivity_priority,
        na.rm = TRUE
      ),
    
    connectivity_priority_with_species =
      sum(
        connectivity_priority &
          focal_species_present,
        na.rm = TRUE
      )
  )


# 3. Which connectivity-priority precincts have focal-species records?
connectivity_species_table <- species_connectivity |>
  filter(
    connectivity_priority
  ) |>
  arrange(
    desc(focal_species_present),
    desc(
      golden_sun_moth_records +
        growling_grass_frog_records
    )
  ) |>
  select(
    psp_name,
    lga,
    golden_sun_moth_records,
    growling_grass_frog_records,
    focal_species_present,
    species_group,
    high_habitat_percent,
    bcs_percent,
    connectivity_importance,
    robust_fragmented,
    high_development
  )

connectivity_species_table |>
  print(
    n = Inf,
    width = Inf
  )


species_connectivity |>
  summarise(
    total_precincts = n(),
    
    precincts_with_focal_species =
      sum(
        focal_species_present,
        na.rm = TRUE
      ),
    
    connectivity_priority_precincts =
      sum(
        connectivity_priority,
        na.rm = TRUE
      ),
    
    connectivity_priority_with_species =
      sum(
        connectivity_priority &
          focal_species_present,
        na.rm = TRUE
      )
  )

species_connectivity |>
  filter(
    connectivity_priority,
    focal_species_present
  ) |>
  select(
    psp_name,
    lga,
    golden_sun_moth_records,
    growling_grass_frog_records,
    species_group,
    high_habitat_percent,
    bcs_percent,
    major_patch_count,
    major_habitat_area_ha,
    mean_nearest_patch_distance_m
  ) |>
  print(
    n = Inf,
    width = Inf
  )

species_connectivity |>
  filter(
    connectivity_priority
  ) |>
  select(
    psp_name,
    lga,
    golden_sun_moth_records,
    growling_grass_frog_records,
    focal_species_present,
    species_group,
    high_habitat_percent,
    bcs_percent
  ) |>
  arrange(
    desc(focal_species_present)
  ) |>
  print(
    n = Inf,
    width = Inf
  )


species_connectivity_summary <- species_connectivity |>
  summarise(
    total_precincts = n(),
    
    focal_species_precincts =
      sum(
        focal_species_present,
        na.rm = TRUE
      ),
    
    connectivity_priority_precincts =
      sum(
        connectivity_priority,
        na.rm = TRUE
      ),
    
    connectivity_priority_with_species =
      sum(
        connectivity_priority &
          focal_species_present,
        na.rm = TRUE
      ),
    
    connectivity_priority_without_species =
      sum(
        connectivity_priority &
          !focal_species_present,
        na.rm = TRUE
      )# 1. Start with all 101 analytical precincts, then add species records
  )

species_connectivity_summary

write_csv(
  species_connectivity,
  "data/processed/species_connectivity_results.csv"
)

write_csv(
  connectivity_species_table,
  "data/processed/connectivity_species_table.csv"
)

write_csv(
  species_connectivity_summary,
  "data/processed/species_connectivity_summary.csv"
)