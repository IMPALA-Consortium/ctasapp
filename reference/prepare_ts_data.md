# Prepare timeseries data for outlier sites

Filters the measures data frame to a single parameter and sites whose
`max_score` exceeds the threshold, selecting key columns for display.

## Usage

``` r
prepare_ts_data(measures, parameter_id, thresh)
```

## Arguments

- measures:

  Data frame as returned by
  [`prepare_measures()`](https://IMPALA-Consortium.github.io/ctasapp/reference/prepare_measures.md).

- parameter_id:

  Character scalar, the parameter to filter on.

- thresh:

  Numeric threshold for outlier flagging.

## Value

A data frame with columns: site, subject_id, timepoint_rank,
timepoint_1_name, result, parameter_name, max_score. Sorted by site,
subject_id, timepoint_rank.
