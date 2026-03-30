# Validate uploaded untransformed file

Checks that a data frame has the join-key columns required to link
untransformed data back to the input measures.

## Usage

``` r
validate_upload_untransformed(df)
```

## Arguments

- df:

  Data frame to validate.

## Value

Character vector of error messages (length-0 means valid).
