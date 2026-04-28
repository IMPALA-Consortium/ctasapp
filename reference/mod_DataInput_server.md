# Data Input Module - Server

Returns a named list of reactives: `measures`, `ctas_results`,
`untransformed`, `queries`, `dataset_label`, `studies`, and
`selected_study`. When the uploaded results file contains a `study`
column with more than one unique value, a study selector is shown on the
Data tab and the chosen study is used to filter both the results and
input data frames before validation/reconstruction.

## Usage

``` r
mod_DataInput_server(id)
```

## Arguments

- id:

  Module namespace ID.

## Value

Named list of reactive expressions.
