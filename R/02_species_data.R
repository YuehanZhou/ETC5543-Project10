library(tidyverse)
library(sf)

# Read study LGA boundaries
lga_boundary <- st_read(
  "data/LGA_2025_AUST_GDA2020/LGA_2025_AUST_GDA2020.shp",
  quiet = TRUE
)

study_lga <- lga_boundary |>
  filter(
    LGA_NAME25 %in% c(
      "Casey", "Hume", "Melton",
      "Whittlesea", "Wyndham"
    )
  )

# Read species data
golden_moth <- read_csv(
  "data/golden_sun_moth_victoria_2026-09-01/golden_sun_moth_victoria_2026-09-01.csv"
)

bandicoot <- read_csv(
  "data/southern_brown_bandicoot_victoria_2026-09-01/southern_brown_bandicoot_victoria_2026-09-01.csv"
)

striped_lizard <- read_csv(
  "data/striped_legless_lizard_victoria_2026-09-01/striped_legless_lizard_victoria_2026-09-01.csv"
)

growling_frog <- read_csv(
  "data/growling_grass_frog_victoria_2026-09-01/growling_grass_frog_victoria_2026-09-01.csv"
)

# Check species data
nrow(golden_moth)
nrow(bandicoot)
nrow(striped_lizard)
nrow(growling_frog)

golden_moth |>
  summarise(
    missing_year = sum(is.na(year)),
    missing_lon = sum(is.na(decimalLongitude)),
    missing_lat = sum(is.na(decimalLatitude))
  )

bandicoot |>
  summarise(
    missing_year = sum(is.na(year)),
    missing_lon = sum(is.na(decimalLongitude)),
    missing_lat = sum(is.na(decimalLatitude))
  )

striped_lizard |>
  summarise(
    missing_year = sum(is.na(year)),
    missing_lon = sum(is.na(decimalLongitude)),
    missing_lat = sum(is.na(decimalLatitude))
  )

growling_frog |>
  summarise(
    missing_year = sum(is.na(year)),
    missing_lon = sum(is.na(decimalLongitude)),
    missing_lat = sum(is.na(decimalLatitude))
  )

# Spatial join
join_to_lga <- function(data, species_name) {
  data |>
    filter(
      !is.na(decimalLongitude),
      !is.na(decimalLatitude)
    ) |>
    st_as_sf(
      coords = c("decimalLongitude", "decimalLatitude"),
      crs = 4326
    ) |>
    st_transform(st_crs(study_lga)) |>
    st_join(
      study_lga |> select(LGA_NAME25),
      left = FALSE
    ) |>
    mutate(species = species_name)
}

golden_lga <- join_to_lga(golden_moth, "Golden Sun Moth")
bandicoot_lga <- join_to_lga(bandicoot, "Southern Brown Bandicoot")
lizard_lga <- join_to_lga(striped_lizard, "Striped Legless Lizard")
frog_lga <- join_to_lga(growling_frog, "Growling Grass Frog")

# Compare candidate species
species_lga <- bind_rows(
  golden_lga |> select(species, LGA_NAME25, geometry),
  bandicoot_lga |> select(species, LGA_NAME25, geometry),
  lizard_lga |> select(species, LGA_NAME25, geometry),
  frog_lga |> select(species, LGA_NAME25, geometry)
)

species_lga |>
  st_drop_geometry() |>
  count(species, LGA_NAME25, sort = TRUE)

species_lga |>
  st_drop_geometry() |>
  count(species, sort = TRUE)

species_lga |>
  st_drop_geometry() |>
  distinct(species, LGA_NAME25) |>
  count(species, name = "n_lga")