# Predicting ongoing transmission and flare-ups in a declining Ebola epidemic: Understanding the relative performance of experts and mathematical models

Analysis code for the paper of the same name.

## Project structure

The work spans two repositories plus the forecasting package:

- This repository evaluates the forecasts: it combines the model forecasts, the reported case outcomes, and the experts' forecasts, then scores and ranks them.
- [Ebola-Expert-Interviews](https://github.com/epiforecasts/Ebola-Expert-Interviews) holds the expert side: the elicited forecasts, cleaned into the `results_*.csv` files this analysis reads. It is included here as a git submodule (it was previously named `Expert-elicitation`).
- The computational forecasts were produced with [EpiCastR](https://github.com/epiforecasts/EpiCastR).

## Getting set up

Clone the repository together with its submodule:

```sh
git clone --recurse-submodules https://github.com/epiforecasts/Ebola-Expert-Elicitation.git
```

If you already cloned without `--recurse-submodules`, pull the expert data in with:

```sh
git submodule update --init
```

Install the R packages the analysis uses:

```r
install.packages(c(
  "data.table", "dplyr", "tidyverse", "lubridate", "stringr", "sf", "raster",
  "rgdal", "DescTools", "ggplot2", "viridis", "patchwork", "cowplot",
  "ggnewscale", "ggbump", "ggbeeswarm", "wesanderson"
))
remotes::install_github("wmgeolab/rgeoboundaries")
```

Regenerating the forecasts (see step 0 below) additionally needs EpiCastR and a Stan toolchain:

```r
remotes::install_github("epiforecasts/EpiCastR")
```

Run the scripts from the root of this repository so the relative paths resolve.

## Data sources

The Ebola case data are third-party humanitarian data from the DRC Ministry of Health and WHO, distributed via the [Humanitarian Data Exchange](https://data.humdata.org/dataset/ebola-cases-and-deaths-drc-north-kivu) under the CC BY-IGO 3.0 licence. See [`data/Ebola/README.md`](data/Ebola/README.md) for full provenance and attribution. The repository `LICENSE` (MIT) covers the analysis code only, not the third-party data.

## How to run

The analysis is a set of scripts run in order. Later scripts reuse functions and data frames that earlier scripts create (such as the `log_score` function and the case time series), so run them in a single R session, from the repository root.

0. (optional) `R/run_forecasts_ee.R` regenerates the computational forecasts with EpiCastR and writes them to `forecasts/new/`. The committed forecasts in `forecasts/` are the ones used for the paper.
1. `R/clean_data_surveyed.R` runs the main analysis, on the health zones included in the survey. It combines the case outcomes, model forecasts and expert forecasts, scores each with the Brier and log scores, and writes `outputs/indevidual_results_with_scores_adj.csv`.
2. `R/score_and_rank_combinations_surveyed.R` reads that file and produces the main figures (scores by health zone, by month and overall, plus the ranking plots) into `plots/`.
3. `R/calculate_hazard_rates_surveyed.R` reads the same scored file and produces the hazard-gap (bias) figures.
4. `R/clean_data_nominated.R` runs the secondary analysis, on the health zones the experts nominated as flare-up risks, and writes `outputs/indevidual_results_with_scores_adj_additional_*.csv`.
5. `R/score_ans_rank_combinations_nominated.R` scores and plots the nominated-zone results, including the introduction maps.

`R/intro_maps.R` and `R/distribution_of_expert_forecast_probs.R` produce supporting figures.

Scored tables are written to `outputs/` and figures to `plots/`.
