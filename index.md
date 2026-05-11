# ctasapp

Interactive Shiny application for exploring results from the
[{ctas}](https://github.com/IMPALA-Consortium/ctas) (Clinical Timeseries
Anomaly Spotter) R package. Provides drill-down visualizations of
site-level outlier scores and subject-level time series data.

## Installation

You can install the development version of ctasapp from
[GitHub](https://github.com/IMPALA-Consortium/ctasapp) with:

``` r

# install.packages("remotes")
remotes::install_github("IMPALA-Consortium/ctasapp")
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

## Configuration

The app ships with a default `ctasapp.yml` that controls colours,
parameter icons, which ctas features are pre-selected, and (optionally)
the location of an embedded dataset to make available in the Data Source
dropdown.

When
[`run_ctas_app()`](https://IMPALA-Consortium.github.io/ctasapp/reference/run_ctas_app.md)
is called without an explicit `config = ...`, the following discovery
chain is used:

1.  `ctasapp.yml` in the current working directory (i.e. next to
    `app.R`).
2.  `system.file("ctasapp.yml", package = "ctasapp")` (the copy shipped
    with the installed package).
3.  Built-in defaults if neither is found.

To customise, copy the default config next to your `app.R` and edit it —
[`run_ctas_app()`](https://IMPALA-Consortium.github.io/ctasapp/reference/run_ctas_app.md)
will discover it automatically via step 1 of the chain above:

``` r

# Copy the shipped default into the app directory
file.copy(
  system.file("ctasapp.yml", package = "ctasapp"),
  "ctasapp.yml"
)

# Edit ctasapp.yml to taste, then launch:
run_ctas_app()
```

Alternatively, pass an explicit path:

``` r

run_ctas_app(config = "/path/to/ctasapp.yml")
```

The config file is YAML with up to four sections:

``` yaml
colors:
  score_breaks: [1.3, 3, 5, 10]
  plot: ["#9ED782", "#fed8019c", "#fed801", "#FEAA01", "#FF5858"]
  table: ["#FFFFFF", "#feed01", "#fed801", "#FEAA01", "#FF5858"]
  table_text: ["#1A1A1A", "#1A1A1A", "#1A1A1A", "#FFFFFF", "#FFFFFF"]
  query_no_change: "#a380e9"
  query_data_change: "#2790e0"

icons:
  range_normalized: flask
  numeric: chart-line
  categorical: water
  bar: chart-bar

features:
  default:
    - autocorr
    - average
    - sd

# Optional embedded dataset shipped with the deployed app
embedded:
  results: data/results.parquet
  input: data/input.parquet
  untransformed: data/untransformed.parquet  # optional
  queries: data/queries.parquet              # optional
```

Any key you omit falls back to the built-in default.

### Embedded datasets (Posit Connect deployments)

When the `embedded:` block is set with at least `results` and `input`,
an **Embedded data set** entry is added to the Data Source dropdown.
Clicking **Load Data** reads those files and pushes them through the
same validation and reconstruction pipeline used for uploads.

Paths in `embedded:` may be absolute, contain `~`, or be **relative to
the directory containing `ctasapp.yml`**. This is the recommended
pattern for Posit Connect deployments — bundle `app.R`, `ctasapp.yml`,
and a `data/` folder together:

``` r

rsconnect::deployApp(
  appFiles = c("app.R", "ctasapp.yml", "data/")
)
```

On the server, `app.R` runs from the bundle root, `ctasapp.yml` is
discovered via step 1 of the chain above, and the relative `data/...`
paths resolve against it. The Data Source dropdown lists the resolved
file paths with a per-file existence marker so operators can verify the
deployment without running a full Load.

## AI Disclaimer

This package and documentation were developed with assistance from AI
tools, including Cursor and Claude Opus 4.6. All AI-generated content
has been reviewed.

## Quality Control

Since {ctas} is designed for use in a
[GCP](https://en.wikipedia.org/wiki/Good_clinical_practice) framework,
we have conducted extensive quality control as part of our development
process. In particular, we do the following during early development:

- **Unit Tests** - Unit tests are written for all core functions, 100%
  coverage required. Lines that cannot be tested in a non-interactive
  context (e.g. Shiny reactive inputs, render callbacks, defensive
  guards that only fire inside a running app) are annotated with
  `# nocov` and excluded from the coverage metric. See the [Contributor
  Guidelines](https://impala-consortium.github.io/ctasapp/articles/ContributorGuidelines.html)
  for the full policy.
- **Workflow Tests** -
  [`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html)
  tests verify that Shiny module logic behaves as expected.
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
  [vignette](https://impala-consortium.github.io/ctasapp/articles/ContributorGuidelines.html).

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
