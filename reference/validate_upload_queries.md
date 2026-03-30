# Validate uploaded queries file

Checks that a data frame has the columns required for query overlay on
plots and display in the queries table.

## Usage

``` r
validate_upload_queries(df)
```

## Arguments

- df:

  Data frame to validate.

## Value

Character vector of error messages (length-0 means valid).
