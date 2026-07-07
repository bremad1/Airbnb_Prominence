# Airbnb Prominence Analysis

This repository contains the R scripts used for the Airbnb prominence/superhost analysis.

## Setup

1. Clone this repository.
2. Put the local data files in the project root or the ignored data folders.
3. Open R/RStudio/Codex from the repository root before running scripts.

Large raw/intermediate data files are generally not tracked by git. The required
`Quarterly_dataset1.RData` and `RData/Entire.RData` files are tracked for this project,
while raw CSV files and generated `results/` outputs should stay local or be copied
separately through iCloud Drive, external storage, or another file-sharing service.

## Main Files

- `3.1 Panel B conditioning_backup_20260706_193434.R`: current working Panel B script.
- `codex/`: Codex helper/search scripts. Run them from the project root or from
  inside `codex/`; they load data through project-relative paths.
- `func/`: helper functions, including RD estimation scripts.
- `tex/`: LaTeX outputs and table material.

## Data Files Not Tracked

The `.gitignore` excludes additional large data and generated artifacts, including:

- `*.RData`
- `*.csv`
- `*.xlsx`
- `RData/`
- `raw CSV/`
- `scrapped data/`
- `results/`

After cloning on another computer, keep any additional local data files in the same
relative locations before running the analysis.

## RData Dependencies From Script 3 Onward

For the current `3.1 Panel B...` scripts, the required data files are:

- `RData/Entire.RData`
- `Quarterly_dataset1.RData`

For the Codex helper/search scripts, the main required data file is:

- `Quarterly_dataset1.RData`

For the broader numbered scripts from `3` onward, the code references:

- `RData/Entire.RData`
- `RData/Quarterly_dataset1.RData`
- `Quarterly_dataset1.RData`
- `Quarterly_dataset2.RData`
- `Quarterly_dataset3.RData`
- `Quarterly_dataset4.RData`
- `RData/Estimation_Results.RData`
- `scrapped data/ratio_flex3.RData`
- `scrapped data/ratio_flex12.RData`

Older scripts `3-2. 3months.R` and `3.3 Ex2.R` reference
`RData/Quarterly_dataset.RData`, but the current local copy has
`Quarterly_dataset.RData` in the project root instead. Either copy it into
`RData/Quarterly_dataset.RData` on the other computer or update those script paths.
