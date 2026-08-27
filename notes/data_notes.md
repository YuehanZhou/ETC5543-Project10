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
- The spatial boundaries were successfully joined to the population data using the 2025 LGA code.