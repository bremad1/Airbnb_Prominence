# Airbnb Prominence Analysis

This repository contains the R scripts used for the Airbnb prominence/superhost analysis.

## Setup

1. Clone this repository.
2. Put the local data files in the project root or the ignored data folders.
3. Open R/RStudio/Codex from the repository root before running scripts.

Large raw/intermediate data files are intentionally not tracked by git. In particular,
files such as `Quarterly_dataset*.RData`, raw CSV files, and generated `results/`
outputs should be copied separately through iCloud Drive, external storage, or another
file-sharing service.

## Main Files

- `3.1 Panel B conditioning_backup_20260706_193434.R`: current working Panel B script.
- `func/`: helper functions, including RD estimation scripts.
- `tex/`: LaTeX outputs and table material.

## Data Files Not Tracked

The `.gitignore` excludes large data and generated artifacts, including:

- `*.RData`
- `*.csv`
- `*.xlsx`
- `RData/`
- `raw CSV/`
- `scrapped data/`
- `results/`

After cloning on another computer, copy the required data files into the same relative
locations before running the analysis.
