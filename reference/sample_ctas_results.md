# Sample ctas analysis results

Output of
[`ctas::process_a_study()`](https://rdrr.io/pkg/ctas/man/process_a_study.html)
run on
[sample_ctas_data](https://IMPALA-Consortium.github.io/ctasapp/reference/sample_ctas_data.md).

## Usage

``` r
sample_ctas_results
```

## Format

A list with four elements:

- timeseries:

  Tibble with 10 rows: timeseries metadata

- timeseries_features:

  Tibble with 7902 rows: per-subject feature values

- PCA_coordinates:

  Tibble: first two principal components per subject

- site_scores:

  Tibble with 2221 rows: site biasness scores per timeseries/feature,
  including fdr_corrected_pvalue_logp

## Source

Generated via `data-raw/sample_data.R`
