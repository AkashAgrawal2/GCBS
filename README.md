# GCBS Shiny App

GCBS is a local R Shiny app for dataset import, cleaning, transformation, joining, statistical-test recommendations, univariate summaries, and figure generation.

## Run the App

1. Install R from <https://cran.r-project.org> if it is not already installed.
2. Download this repository from GitHub and unzip it.
3. Open the app:
   - macOS: double-click `Open_GCBS.command`
   - Windows: double-click `Open_GCBS.bat`

The launcher installs missing R packages the first time it runs, then opens the app in your browser.

## Notes

- The built-in example dataset is simulated demonstration data only.
- It does not contain or represent real patient data, protected health information, or clinical records.
- Imported datasets stay on your computer while running the local app.

## Required R Packages

- `shiny`
- `bslib`
- `ggplot2`
