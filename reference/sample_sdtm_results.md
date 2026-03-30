# Sample SDTM-derived ctas analysis results

Output of
[`ctas::process_a_study()`](https://rdrr.io/pkg/ctas/man/process_a_study.html)
run on
[sample_sdtm_data](https://IMPALA-Consortium.github.io/ctasapp/reference/sample_sdtm_data.md).

## Usage

``` r
sample_sdtm_results
```

## Format

A list with four elements:

- timeseries:

  Tibble: timeseries metadata

- timeseries_features:

  Tibble: per-subject feature values

- PCA_coordinates:

  Tibble: first two principal components per subject

- site_scores:

  Tibble: site biasness scores per timeseries/feature

## Source

Generated via `data-raw/sample_sdtm_data.R`
