# Create ctas input for screening weight categories from VS

Derives weight categories from screening-visit weight measurements and
encodes as a single-timepoint categorical parameter.

## Usage

``` r
Input_BMI(dfDM, dfVS)
```

## Arguments

- dfDM:

  Data frame with SDTM DM columns.

- dfVS:

  Data frame with SDTM VS columns.

## Value

A list with elements `data`, `subjects`, `parameters`, plus
`untransformed` with original weight values and categories.
