# Validate uploaded protocol deviations file

Checks that a data frame has the columns required to display protocol
deviations at site level in the Field Detail view. Only `site` is
required; other columns are passed through as-is.

## Usage

``` r
validate_upload_pd(df)
```

## Arguments

- df:

  Data frame to validate.

## Value

Character vector of error messages (length-0 means valid).
