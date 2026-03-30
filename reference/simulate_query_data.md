# Simulate clinical query data for a ctas input dataset

Generates realistic-looking query records for approximately 15% of data
points. Each query is linked back to the source data via `subject_id`,
`parameter_id`, and `timepoint_1_name`. About 20% of queries indicate a
data change (`data_change = TRUE`).

## Usage

``` r
simulate_query_data(
  ctas_data,
  seed = 123,
  query_frac = 0.04,
  change_frac = 0.2
)
```

## Arguments

- ctas_data:

  A ctas input list with `data` and `parameters` elements.

- seed:

  Integer seed for reproducibility.

- query_frac:

  Fraction of data points to receive queries. Default: 0.04.

- change_frac:

  Fraction of queries that lead to a data change. Default: 0.20.

## Value

A data frame with one row per simulated query.
