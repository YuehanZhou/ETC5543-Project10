# Urban Sprawl and Biodiversity in Melbourne's Growth Corridors

This repository contains the analysis, visualisations, interactive tools and final report for the ETC5543 Business Analytics Creative Activity project on urban expansion and biodiversity across Melbourne's growth corridors.

The project examines where urban development pressure, biodiversity value and gaps in mapped conservation or planning coverage overlap across 101 PSP/MSA precincts in Casey, Hume, Melton, Whittlesea and Wyndham.

## Research question

**Where do urban development pressure, biodiversity value and conservation gaps overlap in Melbourne's growth corridors, and which precincts warrant further assessment of habitat connectivity and planning protection?**

## Project overview

The analysis combines:

- historical urban land-cover change;
- Habitat Value;
- recent Golden Sun Moth and Growling Grass Frog occurrence records;
- Biodiversity Conservation Strategy Conservation Areas;
- Environmental Significance Overlays;
- habitat fragmentation and structural connectivity indicators; and
- an interactive Shiny Priority Explorer.

The project uses two complementary screening approaches:

1. **Baseline ecological screening**, which identifies precincts where development pressure, ecological value and no mapped BCS coverage occur together.
2. **Structural habitat-connectivity screening**, which considers habitat fragmentation, major habitat patches, patch separation and development pressure.

The outputs are intended as an exploratory screening framework for further ecological and planning assessment rather than confirmed wildlife-corridor designations or final planning decisions.

## Repository structure

```text
ETC5543-Project10/
├── R/                  # Data processing, analysis and Shiny scripts
├── data/               # Source and processed project data
├── figures/            # Figures produced for the analysis and report
├── report/             # Quarto report source, bibliography and rendered report
├── agenda/             # Project meeting and planning material
├── README.md
└── ETC5543-Project10.Rproj
```

## Analytical workflow

The main analytical scripts are stored in the `R/` folder.

| File | Purpose |
|---|---|
| `01_data_check.R` | Initial inspection and checking of project data |
| `02_species_data.R` | Preparation of focal-species occurrence records |
| `02_biodiversity_processing.qmd` | Biodiversity data exploration and processing |
| `03_development_processing.R` | Processing of historical urban development indicators |
| `04_master_dataset.R` | Integration of development and biodiversity indicators at precinct level |
| `05_priority_analysis.R` | Baseline ecological screening |
| `06_biodiversity_results.R` | Habitat, species and conservation-gap summaries and figures |
| `07_priority_explorer.R` | Interactive Shiny Priority Explorer |
| `08_fragmentation_connectivity.R` | Habitat fragmentation, connectivity and sensitivity analysis |
| `09_overlay_analysis.R` | Environmental planning-overlay analysis |
| `10_species_connectivity.R` | Integration of focal-species and connectivity evidence |
| `11_final_integration.R` | Final integration of ecological, planning and connectivity outputs |

The numbered scripts reflect the general order of the analytical workflow. Some scripts use outputs created by earlier processing steps.

## Software

The project was developed in R.

Key packages include:

- `tidyverse` for data manipulation and visualisation;
- `sf` for vector spatial analysis;
- `terra` for raster processing;
- `shiny` for the interactive application;
- `leaflet` for interactive mapping; and
- `DT` for interactive tables.

Packages can be installed in R using:

```r
install.packages(c(
  "tidyverse",
  "sf",
  "terra",
  "shiny",
  "leaflet",
  "DT"
))
```

## Data sources

The project uses data from Victorian Government sources, including:

- Victorian Land Cover Time Series;
- Habitat Value;
- Victorian Biodiversity Atlas;
- Biodiversity Conservation Strategy Conservation Areas;
- VPA Precinct Boundaries;
- Melbourne Strategic Assessment Program Precincts; and
- Vicmap Planning Scheme Overlay data.

Detailed descriptions, citations and licensing information for these datasets are provided in the final report.

Some original spatial datasets are too large to store directly in the Git repository. Where source files are not included, the report documents the original provider and dataset source so that the data can be obtained independently.

## Reproducing the analysis

1. Clone or download this repository.
2. Open `ETC5543-Project10.Rproj` in RStudio.
3. Install the required R packages.
4. Ensure the required source datasets are available in the corresponding `data/` directories.
5. Run the analytical scripts in the workflow described above.
6. Generated outputs are saved to the relevant `data/processed/` and `figures/` directories.

The final integrated outputs are produced through the later analysis scripts, particularly `08_fragmentation_connectivity.R`, `09_overlay_analysis.R`, `10_species_connectivity.R` and `11_final_integration.R`.

## Interactive Priority Explorer

The project includes an interactive Shiny application in:

```text
R/07_priority_explorer.R
```

The application allows users to:

- compare all 101 analytical precincts;
- adjust the relative weighting of development pressure, biodiversity importance and conservation gap;
- view precinct-level Priority Scores;
- explore focal-species evidence;
- identify connectivity-priority precincts; and
- examine mapped BCS and ESO coverage.

Priority Scores are relative rankings within the study area and should not be interpreted as probabilities of ecological risk.

## Final report

The final report is prepared in Quarto.

Source file:

```text
report/ETC5543_final_report.qmd
```

Bibliography:

```text
report/references.bib
```

To render the report from the repository root:

```bash
quarto render report/ETC5543_final_report.qmd
```

## Reproducibility notes

The project separates original inputs, processed datasets and generated outputs where possible.

Large source spatial datasets are not necessarily included in the repository because of file-size limitations. Generated HTML and other temporary rendering files are not required to reproduce the core analysis.

The repository should be used together with the dataset access information documented in the report.

## Authors

**Siddhi Jadhav**  
**Yuehan Zhou**

ETC5543 Business Analytics Creative Activity  
Monash University

## Licence

The project source code is distributed under the licence provided in `LICENSE.md`.

Third-party datasets remain subject to the copyright, licensing and usage conditions specified by their original providers.