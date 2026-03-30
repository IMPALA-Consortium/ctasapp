# Cross-validate uploaded files

Checks referential integrity between the results and input files:
parameter_ids in input should appear in results, and sites in results
should appear in input.

## Usage

``` r
validate_upload_crossfile(input_df, results_df)
```

## Arguments

- input_df:

  Input data frame (File 2).

- results_df:

  Results data frame (File 1).

## Value

Character vector of warning messages (length-0 means clean).
