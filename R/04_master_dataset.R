library(tidyverse)
library(sf)

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

# Fix the two PSP names that do not match
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
  st_as_sf()

# Keep only precincts with matched geometry
msa_matched <- msa_spatial |>
  filter(!st_is_empty(geometry))

# Transform to VicGrid
msa_vicgrid <- st_transform(msa_matched, 3111)

study_lga_vicgrid <- st_transform(study_lga, 3111)

# Create one study-area polygon
study_area <- study_lga_vicgrid |>
  st_union()

# Calculate original precinct area
msa_full_area <- msa_vicgrid |>
  mutate(
    full_area_ha =
      as.numeric(st_area(geometry)) / 10000
  ) |>
  st_drop_geometry() |>
  select(
    psp_name,
    full_area_ha
  )

# Clip precincts to the five-LGA study area
msa_study <- suppressWarnings(
  st_intersection(
    msa_vicgrid,
    study_area
  )
)

# Calculate area retained inside the study area
msa_study <- msa_study |>
  mutate(
    study_area_ha =
      as.numeric(st_area(geometry)) / 10000
  ) |>
  left_join(
    msa_full_area,
    by = "psp_name"
  ) |>
  mutate(
    study_overlap_percent =
      study_area_ha / full_area_ha * 100
  )

# Keep precincts with substantial overlap with the study area
msa_study <- msa_study |>
  filter(
    study_overlap_percent >= 50
  )

stopifnot(
  nrow(msa_study) == 101,
  n_distinct(msa_study$psp_name) == 101,
  !anyDuplicated(msa_study$psp_name)
)

library(terra)

# Load Habitat Value raster
habitat <- rast(
  "data/habitat_value/habitat_value_75m_20241024_vg2020.tif"
)

# Transform precincts to the Habitat Value raster CRS
msa_habitat <- st_transform(
  msa_study,
  crs(habitat)
)

# Extract mean Habitat Value for each precinct
habitat_psp <- terra::extract(
  habitat,
  vect(msa_habitat),
  fun = mean,
  na.rm = TRUE
)

# Add precinct names
habitat_psp <- msa_habitat |>
  st_drop_geometry() |>
  select(psp_name) |>
  bind_cols(habitat_psp) |>
  select(-ID)

# Create high-value habitat raster
high_habitat <- habitat >= 70

# Calculate proportion of high-value habitat in each precinct
high_habitat_psp <- terra::extract(
  high_habitat,
  vect(msa_habitat),
  fun = mean,
  na.rm = TRUE
)

# Add precinct names
high_habitat_psp <- msa_habitat |>
  st_drop_geometry() |>
  select(psp_name) |>
  bind_cols(
    high_habitat_psp |> select(-ID)
  )

# Rename and convert proportion to percentage
names(high_habitat_psp)[2] <- "high_habitat_share"

high_habitat_psp <- high_habitat_psp |>
  mutate(
    high_habitat_percent = high_habitat_share * 100
  ) |>
  select(psp_name, high_habitat_percent)

# Read BCS Conservation Areas
bcs <- st_read(
  "data/BCS_Conservation_Areas/ll_gda2020/esrishape/whole_of_dataset/victoria/FLORAFAUNA1/MSA_BCS_CONS_AREA.shp",
  quiet = TRUE
)

# Transform BCS to the same CRS as the precincts
bcs_vicgrid <- st_transform(bcs, st_crs(msa_study))

# Intersect BCS areas with PSP/MSA precincts
bcs_psp <- suppressWarnings(
  st_intersection(
    msa_study |> select(psp_name),
    bcs_vicgrid
  )
)

# Calculate BCS area inside each precinct
bcs_psp_summary <- bcs_psp |>
  mutate(
    bcs_area_ha = as.numeric(st_area(geometry)) / 10000
  ) |>
  st_drop_geometry() |>
  group_by(psp_name) |>
  summarise(
    bcs_area_ha = sum(bcs_area_ha, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate precinct area
psp_area <- msa_study |>
  mutate(
    psp_area_ha = as.numeric(st_area(geometry)) / 10000
  ) |>
  st_drop_geometry() |>
  select(psp_name, psp_area_ha)

# Calculate percentage of each precinct covered by BCS
bcs_psp_summary <- psp_area |>
  left_join(
    bcs_psp_summary,
    by = "psp_name"
  ) |>
  mutate(
    bcs_area_ha = replace_na(bcs_area_ha, 0),
    bcs_percent = bcs_area_ha / psp_area_ha * 100
  ) |>
  select(psp_name, bcs_percent)

# Read selected VBA fauna records
vba_full <- st_read(
  "data/VBA_selected/vba_selected.gpkg",
  quiet = TRUE
)

# Keep recent records of the two focal species
focal_species_recent <- vba_full |>
  filter(
    COMM_NAME %in% c(
      "Golden Sun Moth",
      "Growling Grass Frog"
    ),
    START_YEAR >= 2015
  )

# Match the precinct coordinate system
focal_species_psp <- st_transform(
  focal_species_recent,
  st_crs(msa_study)
)

# Assign records to the analytical precincts
species_psp_join <- st_join(
  focal_species_psp,
  msa_study |> select(psp_name),
  join = st_within,
  left = FALSE
)

# Count records by precinct and species
species_psp_counts <- species_psp_join |>
  st_drop_geometry() |>
  count(
    psp_name,
    COMM_NAME,
    name = "n"
  ) |>
  pivot_wider(
    names_from = COMM_NAME,
    values_from = n,
    values_fill = 0
  ) |>
  rename(
    golden_sun_moth_records = `Golden Sun Moth`,
    growling_grass_frog_records = `Growling Grass Frog`
  )

stopifnot(
  !anyDuplicated(species_psp_counts$psp_name)
)

# Read processed development indicators
psp_urban_growth <- read_csv(
  "data/processed/psp_urban_growth.csv",
  show_col_types = FALSE
)

study_precinct_names <- msa_study |>
  st_drop_geometry() |>
  distinct(psp_name)

psp_urban_growth_study <- psp_urban_growth |>
  semi_join(
    study_precinct_names,
    by = "psp_name"
  )

stopifnot(
  nrow(psp_urban_growth_study) == 101,
  setequal(
    psp_urban_growth_study$psp_name,
    study_precinct_names$psp_name
  )
)

# Combine development and biodiversity indicators
master_psp <- psp_urban_growth_study |>
  left_join(
    habitat_psp,
    by = "psp_name"
  ) |>
  left_join(
    high_habitat_psp,
    by = "psp_name"
  ) |>
  left_join(
    bcs_psp_summary,
    by = "psp_name"
  ) |>
  left_join(
    species_psp_counts,
    by = "psp_name"
  ) |>
  mutate(
    golden_sun_moth_records =
      replace_na(golden_sun_moth_records, 0),
    growling_grass_frog_records =
      replace_na(growling_grass_frog_records, 0)
  )

stopifnot(
  nrow(master_psp) == 101,
  n_distinct(master_psp$psp_name) == 101,
  !anyDuplicated(master_psp$psp_name)
)

# Save master dataset
write_csv(
  master_psp,
  "data/processed/master_psp.csv"
)