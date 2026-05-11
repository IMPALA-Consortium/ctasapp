# Check whether the embedded dataset feature is configured

Returns `TRUE` when
[`get_embedded_paths()`](https://IMPALA-Consortium.github.io/ctasapp/reference/get_embedded_paths.md)
is non-`NULL`, i.e. the operator has set the `embedded:` block in
`ctasapp.yml`. File existence on disk is not checked here; missing files
are surfaced as errors when the user clicks Load.

## Usage

``` r
embedded_files_configured()
```

## Value

Logical scalar.
