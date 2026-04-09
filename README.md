# Universal Statistical Analysis Shiny App

A Shiny dashboard for end-to-end statistical analysis, including descriptive summaries, hypothesis testing, regression modeling, and model diagnostics.

## Features

- Upload your own dataset (`.csv`, `.xlsx`, `.xls`)
- Automatic variable type detection (numeric/categorical)
- Data preview and summary tables
- Variable-level analysis and visualizations
- Correlation matrix and categorical analysis suite
- Hypothesis testing tools
- Simple and multiple linear regression
- Indicator variable modeling
- Model adequacy checks
- Box-Cox and Box-Tidwell transformations
- Weighted least squares workflow
- Downloadable summary report
- Session save/load (`.rds`)

## Default Startup Data

When the app starts, it loads the default dataset from:

`data/P2_DeliveryTime.xlsx`

If that file is unavailable, the app falls back to an internal sample dataset.

## Project Structure

- `app.R` - Main Shiny application
- `data/P2_DeliveryTime.xlsx` - Default startup dataset
- `setup_shinyapps.R` - Optional deployment helper script

## Requirements

Install R packages used by the app:

```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "readxl", "moments", "corrplot",
  "RColorBrewer", "BSDA", "plotrix", "ggplot2", "car", "MASS",
  "lmtest", "bslib", "plotly", "shinycssloaders"
))
```

Notes:
- `plotly` and `shinycssloaders` are optional but recommended for interactive plots and loading spinners.

## Run Locally

From the repository root:

```r
shiny::runApp()
```

Or from terminal:

```bash
Rscript -e "shiny::runApp()"
```

## Deployment Notes

- Keep the `data/` folder in the repository so the default dataset is available in deployed environments.
- If you do not want to include a default file, the app still runs using fallback data.

## License

Add your preferred license here (for example, MIT).
