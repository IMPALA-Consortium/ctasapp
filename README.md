
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ctasapp

<!-- badges: start -->

[![R-CMD-check](https://github.com/IMPALA-Consortium/ctas_app/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/IMPALA-Consortium/ctas_app/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Interactive Shiny application for exploring results from the
[{ctas}](https://github.com/IMPALA-Consortium/ctas) (Clinical Timeseries
Anomaly Spotter) R package. Provides drill-down visualizations of
site-level outlier scores and subject-level time series data.

## Installation

You can install the development version of ctasapp from
[GitHub](https://github.com/IMPALA-Consortium/ctas_app) with:

``` r
# install.packages("remotes")
remotes::install_github("IMPALA-Consortium/ctas_app")
```

## Usage

Launch the app with bundled sample data:

``` r
library(ctasapp)
run_ctas_app()
```

Once the app is running, click **Use Sample Data** on the Data tab, then
switch to the **Fields** tab to explore parameter-level timeseries and
site outlier scores.

## Quality Control

Since {ctas} is designed for use in a
[GCP](https://en.wikipedia.org/wiki/Good_clinical_practice) framework,
we have conducted extensive quality control as part of our development
process. In particular, we do the following during early development:

- **Unit Tests** - Unit tests are written for all core functions, 100%
  coverage required.
- **Workflow Tests** - `shiny::testServer()` tests verify that Shiny
  module logic behaves as expected.
- **Function Documentation** - Detailed documentation for each exported
  function is maintained with Roxygen.
- **Package Checks** - Standard package checks are run using GitHub
  Actions and must be passing before PRs are merged.
- **Continuous Integration** - Continuous integration is provided via
  GitHub Actions.
- **Code Formatting** - Code is formatted with {styler} before each
  release.
- **Contributor Guidelines** - Contributor guidelines including
  step-by-step processes for code development are provided as a
  [vignette](https://impala-consortium.github.io/ctas_app/articles/ContributorGuidelines.html).

### Parking

As development progresses, we will also conduct the following quality
control steps:

- **Qualification Workflow** - All assessments will be Qualified as
  described in a Qualification Workflow Vignette. A Qualification Report
  Vignette will be generated and attached to each release.
- **Code Review** - Code review is conducted using GitHub Pull Requests
  (PRs), and a log of all PRs will be included in the Qualification
  Report Vignette.
- **Regression Testing** - Extensive QC and testing will be done before
  each release.
- **End-to-End Tests** - Browser-based tests using {shinytest2} will
  validate the full user interaction flow.
