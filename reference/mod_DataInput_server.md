# Data Input Module - Server

Returns a named list of reactives: `measures`, `ctas_results`,
`untransformed`, `queries`, `dataset_label`, and `studies`. No study
filtering is applied here; the Fields module owns filtering.

## Usage

``` r
mod_DataInput_server(id)
```

## Arguments

- id:

  Module namespace ID.

## Value

Named list of reactive expressions.
