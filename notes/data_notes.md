# Data Notes

## Population
- 125 rows, 5 councils, 2001–2025.
- No missing values.
- Melton and Wyndham show strong population growth.

## Population components
- 200 rows, 2022–2025.
- 10 population components.
- OBS_STATUS and OBS_COMMENT are entirely missing.

## KBA
- 42 records.
- Contains x/y coordinates but no polygon geometry.
- Covers locations across Victoria.

## MSA precincts
- 122 precinct records.
- Northern, Southern and Western subregions.
- Some precincts cross multiple councils.
- No spatial geometry in the CSV.

## Fauna
- 141 records, 28 species.
- Records span 1982–2022.
- Sampling is strongly concentrated in 2017–2019.
- Species record counts are uneven.

## Flora
- 51 records, 27 species.
- Records span 1979–2023.
- `totalcount` and `coverabund` have substantial missing data.
- Coordinates and FFG status are complete.

## LGA boundaries
- ABS 2025 LGA spatial boundaries were successfully loaded.
- The national dataset contains 567 LGAs.
- Five study LGAs were selected: Casey, Hume, Melton, Whittlesea and Wyndham.

## PSP / MSA spatial matching
- The downloaded PSP boundary layer contains 119 spatial features.
- The MSA attribute table contains 122 records.
- Two obvious naming differences were standardised:
  - Quandong PSP (Council-led) → Quandong PSP
  - Croskell → Croskell (Casey Central)
- After matching by PSP name, 117 MSA records have spatial geometry.
- Five MSA records remain without matched geometry:
  - Officer Town Centre Review
  - Merrifield North Masterplan
  - Cardinia Road Corridor (Review)
  - Pound Road
  - Mernda Town Centre
- Two PSP boundary features are not present in the MSA attribute table:
  - Clyde South
  - Greenvale North (Part 2)
- The spatial boundaries were successfully joined to the population data using the 2025 LGA code.

## Victorian Land Cover Time Series
- Victorian statewide raster land-cover dataset downloaded successfully.
- Contains seven time periods:
  - 1985–1990
  - 1990–1995
  - 1995–2000
  - 2000–2005
  - 2005–2010
  - 2010–2015
  - 2015–2019
- The main data are stored as GeoTIFF raster files.
- Can be used to examine how land cover has changed over time within the study LGAs.
- Particularly useful for identifying areas where non-urban land may have changed to built-up/urban land.
- Provides a more direct measure of physical land-cover change than population growth alone.
- The latest available period ends in 2019, so it does not capture development between 2019 and 2026.
- This dataset will need to be clipped to Casey, Hume, Melton, Whittlesea and Wyndham before analysis.

## Habitat Value
- Victorian Habitat Value spatial raster downloaded successfully.
- The dataset is provided as a 75 m GeoTIFF raster.
- It represents modelled relative biodiversity/habitat value across Victoria.
- Can be used to identify areas of higher biodiversity importance within the study LGAs.
- Potentially useful for overlaying with land-cover change and development-pressure layers to identify locations where urban expansion may affect higher-value habitat.
- Habitat Value is not a direct measure of fauna movement or ecological connectivity.
- A high Habitat Value does not by itself confirm that a particular threatened species is currently present at that location.
- The raster will need to be clipped to the selected study LGAs before spatial analysis.

## BCS Conservation Areas
- Biodiversity Conservation Strategy (BCS) Conservation Areas spatial dataset downloaded successfully.
- The dataset is provided as an ESRI Shapefile with polygon geometry.
- It identifies designated conservation areas within Melbourne's growth corridors.
- Can be used to compare existing conservation areas with urban-development pressure, land-cover change, Habitat Value and threatened-fauna records.
- Useful for identifying whether areas of high biodiversity importance are already within recognised conservation areas or may fall outside them.
- The BCS Conservation Areas layer represents existing conservation planning and is not itself a direct measure of habitat connectivity.
- The dataset is already stored in the GitHub repository.