library(tidyverse)
library(sf)
library(ggplot2)

master_psp <- read_csv(
  "data/processed/master_psp.csv",
  show_col_types = FALSE
)

priority_analysis <- master_psp |>
  mutate(
    focal_species_present =
      golden_sun_moth_records > 0 |
      growling_grass_frog_records > 0
  )

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

priority_analysis <- priority_analysis |>
  mutate(
    high_development =
      growth_ha > development_growth_median |
      urban_share_change > development_share_median,
    
    high_biodiversity =
      high_habitat_percent > habitat_median |
      focal_species_present,
    
    no_bcs_protection =
      bcs_percent == 0,
    
    priority_area =
      high_development &
      high_biodiversity &
      no_bcs_protection
  )

priority_analysis |>
  count(priority_area)

psp_boundary <- st_read(
  "data/PSP_Boundary/PSP_Boundary.geojson",
  quiet = TRUE
)

psp_boundary <- psp_boundary |>
  mutate(
    PSP_NAME = case_when(
      PSP_NAME == "Quandong PSP (Council-led)" ~ "Quandong PSP",
      PSP_NAME == "Croskell" ~ "Croskell (Casey Central)",
      TRUE ~ PSP_NAME
    )
  ) |>
  st_make_valid()

lga_boundary <- st_read(
  "data/LGA_2025_AUST_GDA2020/LGA_2025_AUST_GDA2020.shp",
  quiet = TRUE
)

study_lga <- lga_boundary |>
  filter(
    LGA_NAME25 %in% c(
      "Casey",
      "Hume",
      "Melton",
      "Whittlesea",
      "Wyndham"
    )
  ) |>
  st_make_valid()

study_lga_psp_crs <- st_transform(
  study_lga,
  st_crs(psp_boundary)
)

study_area <- st_union(
  study_lga_psp_crs
)

priority_map <- psp_boundary |>
  inner_join(
    priority_analysis,
    by = c("PSP_NAME" = "psp_name")
  )

priority_map <- st_intersection(
  priority_map,
  study_area
) |>
  st_make_valid()

n_distinct(priority_map$PSP_NAME)

priority_map |>
  st_drop_geometry() |>
  count(priority_area)

priority_map <- priority_map |>
  mutate(
    priority_group = case_when(
      priority_area ~ "Integrated priority precinct",
      TRUE ~ "Other study precinct"
    )
  )

integrated_priority_map <- ggplot() +
  geom_sf(
    data = priority_map,
    aes(fill = priority_group),
    colour = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = study_lga_psp_crs,
    fill = NA,
    colour = "black",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "Integrated priority precinct" = "#C0392B",
      "Other study precinct" = "#D9D9D9"
    ),
    name = NULL
  ) +
  labs(
    title =
      "Integrated priority precincts across Melbourne's fringe growth corridors",
    subtitle =
      "Priority precincts meet the development, biodiversity and BCS criteria",
    caption =
      "Study area: Casey, Hume, Melton, Whittlesea and Wyndham"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    legend.position = "bottom",
    plot.caption = element_text(
      colour = "grey40"
    )
  )

integrated_priority_map

dir.create(
  "figures",
  showWarnings = FALSE
)

ggsave(
  filename =
    "figures/integrated_priority_precincts.png",
  plot =
    integrated_priority_map,
  width = 10,
  height = 8,
  dpi = 300
)

priority_analysis |>
  filter(priority_area) |>
  select(
    psp_name,
    lga,
    growth_ha,
    urban_share_change,
    high_habitat_percent,
    bcs_percent,
    golden_sun_moth_records,
    growling_grass_frog_records
  ) |>
  arrange(desc(growth_ha)) |>
  print(n = Inf)