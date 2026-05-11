# Get embedded dataset file paths from configuration

Returns the list of resolved file paths configured in the `embedded:`
block of `ctasapp.yml`, or `NULL` when the feature is not configured
(i.e. neither `results` nor `input` was set).

## Usage

``` r
get_embedded_paths()
```

## Value

A named list with elements `results`, `input`, `untransformed`,
`queries` (each a character path or `NULL`), or `NULL` if disabled.
