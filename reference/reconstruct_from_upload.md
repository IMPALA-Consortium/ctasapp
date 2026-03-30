# Reconstruct ctas data structures from flat upload files

Takes the flat uploaded data frames and splits them back into the list
structures expected by
[`prepare_measures()`](https://IMPALA-Consortium.github.io/ctasapp/reference/prepare_measures.md).

## Usage

``` r
reconstruct_from_upload(
  input_df,
  results_df,
  untransformed_df = NULL,
  queries_df = NULL
)
```

## Arguments

- input_df:

  Data frame from the input file upload.

- results_df:

  Data frame from the results file upload.

- untransformed_df:

  Optional data frame from the untransformed upload.

- queries_df:

  Optional data frame from the queries upload.

## Value

A named list with `ctas_data` and `ctas_results`.
