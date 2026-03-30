# Prepare wide score table for a single parameter

Pivots per-feature site scores into a wide data frame with one row per
site and one column per feature, plus an aggregated `max_score` column.

## Usage

``` r
prepare_score_table(ctas_results, parameter_id)
```

## Arguments

- ctas_results:

  List as returned by
  [`ctas::process_a_study()`](https://rdrr.io/pkg/ctas/man/process_a_study.html).

- parameter_id:

  Character scalar, the parameter to filter on.

## Value

A data frame with columns: site, one per feature, max_score. Sorted by
descending max_score.
