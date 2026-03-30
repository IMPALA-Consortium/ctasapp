# Create ctas input from SDTM VS domain

Produces numeric vital signs timeseries for each test code. Recalculates
timepoint ranks and filters unscheduled visits.

## Usage

``` r
Input_VS(dfDM, dfVS)
```

## Arguments

- dfDM:

  Data frame with SDTM DM columns.

- dfVS:

  Data frame with SDTM VS columns.

## Value

A list with elements `data`, `subjects`, `parameters`, plus
`untransformed` with original vital sign values.
