# Validate uploaded results file

Checks that a data frame has the columns and types expected for the
pre-joined `site_scores + timeseries` results file.

## Usage

``` r
validate_upload_results(df)
```

## Arguments

- df:

  Data frame to validate.

## Value

Character vector of error messages (length-0 means valid).
