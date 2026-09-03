library(tidyverse)
library(sf)
library(ggplot2)

# Read data
population <- read_csv("data/meeting1/abs_erp_population_2001_2025.csv")
lga_reference <- read_csv("data/meeting1/abs_lga_2025_study_council_code_reference.csv")
population_components <- read_csv("data/meeting1/abs_population_components_2022_2025.csv")
kba <- read_csv("data/meeting1/key_biodiversity_area_attributes.csv")
msa <- read_csv("data/meeting1/msa_precinct_attributes.csv")
fauna <- read_csv("data/meeting1/vba_threatened_fauna_melbourne_fringe.csv")
flora <- read_csv("data/meeting1/vba_threatened_flora_melbourne_fringe.csv")
lga_boundary <- st_read("data/LGA_2025_AUST_GDA2020/LGA_2025_AUST_GDA2020.shp")

# Population
glimpse(population)

population |>
  count(council)

colSums(is.na(population))
range(population$year)

population |>
  filter(year %in% c(2001, 2025)) |>
  arrange(council, year)

# LGA reference
glimpse(lga_reference)
lga_reference

# Population components
glimpse(population_components)

population_components |>
  distinct(component_label)

population_components |>
  count(component_label)

colSums(is.na(population_components))

population_components |>
  select(council, TIME_PERIOD, component_label, OBS_VALUE) |>
  filter(component_label %in% c(
    "Natural Increase",
    "Net Internal Migration",
    "Net Overseas Migration"
  )) |>
  pivot_wider(
    names_from = component_label,
    values_from = OBS_VALUE
  )

# Key Biodiversity Areas
glimpse(kba)
head(kba)

kba |>
  select(name, x, y, hectares, locdescr, triggers)

kba |>
  select(name, locdescr) |>
  print(n = 42)

#  MSA precinct attributes
glimpse(msa)

msa |>
  count(subregion)

msa |>
  count(lga, sort = TRUE)

msa |>
  distinct(type)

msa |>
  select(subregion, lga, psp_name, type) |>
  head(20)

# VBA fauna
glimpse(fauna)

fauna |>
  count(comm_name, sci_name, sort = TRUE)

fauna |>
  count(ffg_desc, sort = TRUE)

range(fauna$start_year, na.rm = TRUE)

fauna |>
  count(reliability)

fauna |>
  count(comm_name, sci_name, sort = TRUE) |>
  print(n = 28)

fauna |>
  count(start_year) |>
  arrange(start_year) |>
  print(n = Inf)

# VBA flora
glimpse(flora)

flora |>
  count(comm_name, sci_name, sort = TRUE) |>
  print(n = Inf)

range(flora$start_year, na.rm = TRUE)

flora |>
  count(ffg_desc, sort = TRUE)

flora |>
  select(
    comm_name, sci_name, start_year, longitude, latitude, totalcount, coverabund, ffg_desc, epbc_desc
  ) |>
  summarise(across(everything(), ~sum(is.na(.))))

# LGA boundary
glimpse(lga_boundary) 
names(lga_boundary)

study_lga <- lga_boundary |>
  filter(LGA_NAME25 %in% c(
    "Casey", "Hume", "Melton", "Whittlesea", "Wyndham"
  ))

study_lga |>
  select(LGA_CODE25, LGA_NAME25, AREASQKM)

ggplot(study_lga) +
  geom_sf() +
  geom_sf_text(aes(label = LGA_NAME25)) +
  labs(
    title = "Study LGAs",
    x = NULL,
    y = NULL
  ) +
  theme_minimal()