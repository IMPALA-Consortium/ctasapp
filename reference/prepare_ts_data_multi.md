# Prepare timeseries data for outlier sites (multiple parameters)

Like
[`prepare_ts_data()`](https://IMPALA-Consortium.github.io/ctasapp/reference/prepare_ts_data.md)
but accepts a vector of parameter IDs. When `untransformed` is supplied
(non-NULL), joins in the original pre-transformation values
(original_value, lower, upper, original_category) keyed on
`subject_id + parameter_category_2 + timepoint_1_name`. Any additional
columns present in `untransformed` (beyond the join keys and the
standard fields) are passed through to the output so they appear in the
Source Data table.

## Usage

``` r
prepare_ts_data_multi(
  measures,
  parameter_ids,
  thresh,
  untransformed = NULL,
  plot_type = "numeric",
  sites = NULL
)
```

## Arguments

- measures:

  Data frame as returned by
  [`prepare_measures()`](https://IMPALA-Consortium.github.io/ctasapp/reference/prepare_measures.md).

- parameter_ids:

  Character vector of parameter IDs.

- thresh:

  Numeric threshold for outlier flagging.

- untransformed:

  Optional data frame with columns subject_id, parameter_category_2,
  timepoint_1_name, original_value, lower, upper, original_category,
  plus any extra user columns to pass through. Pass NULL (default) to
  show transformed result only.

- plot_type:

  Character scalar: "numeric", "categorical", or "bar". Controls column
  selection and deduplication.

- sites:

  Optional character vector of site names. When provided, these sites
  are used directly instead of computing outlier sites from `thresh`.

## Value

A data frame sorted by site, subject_id, timepoint_rank.
