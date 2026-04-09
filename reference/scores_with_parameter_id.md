# Ensure parameter_id is present in site_scores

For uploaded data, `site_scores` already contains `parameter_id`. For
sample/ctas-package data, `parameter_id` lives in the separate
`timeseries` table and must be joined in via `timeseries_id`.

## Usage

``` r
scores_with_parameter_id(ctas_results)
```

## Arguments

- ctas_results:

  List with `site_scores` and optionally `timeseries`.

## Value

A data frame of site scores with a `parameter_id` column.
