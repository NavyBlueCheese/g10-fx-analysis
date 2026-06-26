# G10 FX Analysis

A statistical study of G10 foreign exchange return dynamics, written in R.

Two years of daily spot rates are pulled from the [Frankfurter API](https://frankfurter.dev)
(no API key needed). The notebook then runs a set of classic quantitative checks:
correlation structure, return distributions, rolling Sharpe ratios, and volatility
clustering.

## Key Questions Explored

- Which G10 currencies move together? How strong are the cross-currency correlations?
- How fat are the tails compared with a Normal distribution?
- How stable is the risk-adjusted return over rolling 30-day windows?
- Is there evidence of volatility clustering (a precondition for GARCH models)?

## What the Notebook Produces

| Section | Output |
|---|---|
| Summary statistics | Annualised return, vol, Sharpe, skewness, excess kurtosis per currency |
| Correlation heatmap | Full-period pairwise Pearson correlations, hierarchically ordered |
| Return distributions | Per-currency histograms with Normal fit overlay |
| Rolling Sharpe | 30-day annualised Sharpe vs USD for each currency |
| Volatility clustering | Absolute returns over time with LOESS trend |

## Running It

Install dependencies once:

```r
install.packages(c(
  "jsonlite", "dplyr", "tidyr", "ggplot2",
  "purrr", "lubridate", "patchwork", "scales", "zoo"
))
```

Then knit to GitHub-flavoured Markdown (creates `analysis.md` with embedded charts):

```r
rmarkdown::render("analysis.Rmd")
```

Or open `analysis.Rmd` in RStudio and press **Knit**.

Requires R 4.1+. No API key or local data files needed, everything is fetched live.

## Tech Stack

- **R** 4.1+
- **tidyverse** (dplyr, ggplot2, tidyr, purrr)
- **jsonlite** for API calls
- **zoo** for rolling-window calculations
- **patchwork** and **scales** for plot composition
- **lubridate** for date arithmetic

## Project Structure

```
g10-fx-analysis/
├── analysis.Rmd          # Main notebook (source)
├── analysis.md           # Rendered output with embedded charts (auto-generated)
└── .github/
    └── workflows/
        └── render.yml    # GitHub Action: re-renders on every push to main
```

## Disclaimer

For educational purposes only. Nothing here constitutes financial advice.

---

© 2026 · Made by NavyBlueCheese
