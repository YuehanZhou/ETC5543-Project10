library(tidyverse)
library(sf)
library(leaflet)

# Read combined development and biodiversity data
explorer_data <- read_csv(
  "data/processed/priority_explorer_data.csv"
)

# Read PSP boundaries
psp_boundary <- st_read(
  "data/PSP_Boundary/PSP_Boundary.geojson",
  quiet = TRUE
)

# Standardise PSP names so they match the analysis dataset
psp_boundary <- psp_boundary |>
  mutate(
    PSP_NAME = case_when(
      PSP_NAME == "Quandong PSP (Council-led)" ~ "Quandong PSP",
      PSP_NAME == "Croskell" ~ "Croskell (Casey Central)",
      TRUE ~ PSP_NAME
    )
  )

# Join analysis results to precinct polygons
explorer_map <- psp_boundary |>
  left_join(
    explorer_data,
    by = c("PSP_NAME" = "psp_name")
  ) |>
  filter(!is.na(lga))

# Clean spatial geometry
explorer_map <- explorer_map |>
  st_make_valid()

# Convert to latitude/longitude for Leaflet
explorer_map <- st_transform(
  explorer_map,
  4326
)

stopifnot(
  nrow(explorer_data) == 101,
  n_distinct(explorer_data$psp_name) == 101,
  n_distinct(explorer_map$PSP_NAME) == 101,
  setequal(
    explorer_map$PSP_NAME,
    explorer_data$psp_name
  )
)

# Create popup information for each precinct
explorer_map <- explorer_map |>
  mutate(
    popup_text = paste0(
      "<strong>", PSP_NAME, "</strong><br><br>",
      
      "<strong>LGA:</strong> ",
      lga,
      "<br>",
      
      "<strong>Urban growth:</strong> ",
      round(growth_ha, 1),
      " ha<br>",
      
      "<strong>Urban share change:</strong> ",
      round(urban_share_change, 1),
      " percentage points<br><br>",
      
      "<strong>High-value habitat:</strong> ",
      round(high_habitat_percent, 1),
      "%<br>",
      
      "<strong>High-value habitat outside BCS:</strong> ",
      round(high_habitat_outside_bcs_ha, 1),
      " ha<br>",
      
      "<strong>High-value habitat outside BCS:</strong> ",
      round(high_habitat_outside_bcs_percent, 1),
      "%<br><br>",
      
      "<strong>Golden Sun Moth records:</strong> ",
      golden_sun_moth_records,
      "<br>",
      
      "<strong>Growling Grass Frog records:</strong> ",
      growling_grass_frog_records,
      "<br>",
      
      "<strong>Focal species:</strong> ",
      species_group
    )
  )

# Colour scales
development_pal <- colorNumeric(
  palette = "YlOrRd",
  domain = explorer_map$growth_ha,
  na.color = "transparent"
)

biodiversity_pal <- colorNumeric(
  palette = "YlGn",
  domain = explorer_map$high_habitat_percent,
  na.color = "transparent"
)

gap_pal <- colorNumeric(
  palette = "YlGnBu",
  domain = explorer_map$high_habitat_outside_bcs_ha,
  na.color = "transparent"
)

# Study-area extent
map_bbox <- st_bbox(explorer_map)

# Interactive explorer
priority_explorer_map <- leaflet(
  explorer_map,
  options = leafletOptions(
    preferCanvas = TRUE
  )
) |>
  
  addProviderTiles(
    providers$Esri.WorldGrayCanvas
  ) |>
  
  # Development pressure layer
  addPolygons(
    group = "Development pressure",
    color = "white",
    weight = 1,
    fillColor = ~development_pal(growth_ha),
    fillOpacity = 0.75,
    popup = ~popup_text,
    label = ~PSP_NAME,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) |>
  
  # Biodiversity importance layer
  addPolygons(
    group = "Biodiversity importance",
    color = "white",
    weight = 1,
    fillColor = ~biodiversity_pal(high_habitat_percent),
    fillOpacity = 0.75,
    popup = ~popup_text,
    label = ~PSP_NAME,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) |>
  
  # Habitat outside BCS layer
  addPolygons(
    group = "Habitat outside BCS",
    color = "white",
    weight = 1,
    fillColor = ~gap_pal(high_habitat_outside_bcs_ha),
    fillOpacity = 0.75,
    popup = ~popup_text,
    label = ~PSP_NAME,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) |>
  
  # Show one metric at a time
  addLayersControl(
    baseGroups = c(
      "Development pressure",
      "Biodiversity importance",
      "Habitat outside BCS"
    ),
    options = layersControlOptions(
      collapsed = FALSE
    )
  ) |>
  
  # Development legend
  addLegend(
    group = "Development pressure",
    position = "bottomright",
    pal = development_pal,
    values = ~growth_ha,
    title = "Urban growth (ha)"
  ) |>
  
  # Biodiversity legend
  addLegend(
    group = "Biodiversity importance",
    position = "bottomright",
    pal = biodiversity_pal,
    values = ~high_habitat_percent,
    title = "High-value habitat (%)"
  ) |>
  
  # Habitat outside BCS legend
  addLegend(
    group = "Habitat outside BCS",
    position = "bottomright",
    pal = gap_pal,
    values = ~high_habitat_outside_bcs_ha,
    title = "High-value habitat<br>outside BCS (ha)"
  ) |>
  
  fitBounds(
    lng1 = as.numeric(map_bbox["xmin"]),
    lat1 = as.numeric(map_bbox["ymin"]),
    lng2 = as.numeric(map_bbox["xmax"]),
    lat2 = as.numeric(map_bbox["ymax"])
  )

priority_explorer_map