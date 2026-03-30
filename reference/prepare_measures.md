# Prepare measures table from ctas input and results

Joins the raw timeseries data with subject metadata, parameter metadata,
and aggregated site scores into a single data frame suitable for
plotting.

## Usage

``` r
prepare_measures(ctas_data, ctas_results)
```

## Arguments

- ctas_data:

  List with elements `data`, `subjects`, `parameters` as used by
  [`ctas::process_a_study()`](https://rdrr.io/pkg/ctas/man/process_a_study.html).

- ctas_results:

  List with elements `timeseries`, `timeseries_features`,
  `PCA_coordinates`, `site_scores` as returned by
  [`ctas::process_a_study()`](https://rdrr.io/pkg/ctas/man/process_a_study.html).

## Value

A data frame with one row per subject/timepoint/parameter observation
and columns: subject_id, timepoint_rank, timepoint_1_name, result,
parameter_id, site, country, region, parameter_name,
parameter_category_1/2/3, max_score, n_subjects.
