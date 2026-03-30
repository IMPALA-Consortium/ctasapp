# Validate uploaded input file

Checks that a data frame has the columns and types expected for the
pre-joined `data + subjects + parameters` input file.

## Usage

``` r
validate_upload_input(df)
```

## Arguments

- df:

  Data frame to validate.

## Value

Character vector of error messages (length-0 means valid).
