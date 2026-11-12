# Perceived Control, Cultural Values and Mood

Annotated R code reproducing the analyses from:

Senan, S., Msetfi, R. M., El Keshky, M., & Halbrook, Y. (2019). The relationship between perception of control and mood: The intervening effect of cultural values in a Saudi Arabian sample. *PLOS ONE, 14*(8), e0220509. https://doi.org/10.1371/journal.pone.0220509

## What it does
Data screening, scale scoring and reliability, descriptives and correlations, assumption checks, parallel mediation (equivalent to PROCESS Model 4, 5,000 bootstrap resamples), moderation with simple slopes, and a sensitivity analysis.

## Running it
1. Place the data file at `data/Perception_of_control_study.xlsx` (not included; available on request).
2. Install the required package: `install.packages("readxl")` (optional: `apaTables`).
3. Run `perceived_control_analysis.R`. Results are saved to `output/`.

Built with R 4.6.2.
