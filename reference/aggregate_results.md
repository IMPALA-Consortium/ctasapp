# Aggregate uploaded results by removing timeseries_id

Drops the `timeseries_id` column and collapses rows by taking the
maximum `fdr_corrected_pvalue_logp` per group of remaining columns. This
dramatically reduces the size of the results data frame for large
uploads where many timeseries map to the same parameter/site/feature.

## Usage

``` r
aggregate_results(results_df)
```

## Arguments

- results_df:

  Data frame read from the uploaded results file.

## Value

A data frame without `timeseries_id`, with one row per unique
combination of the remaining columns.
