# Generate sample upload CSV files

Creates the 4 flat CSV files (results, input, untransformed, queries)
that can be uploaded to the ctasapp Shiny dashboard. The files are
generated from the bundled
[sample_ctas_data](https://IMPALA-Consortium.github.io/ctasapp/reference/sample_ctas_data.md),
[sample_ctas_results](https://IMPALA-Consortium.github.io/ctasapp/reference/sample_ctas_results.md),
[sample_sdtm_data](https://IMPALA-Consortium.github.io/ctasapp/reference/sample_sdtm_data.md),
and
[sample_sdtm_results](https://IMPALA-Consortium.github.io/ctasapp/reference/sample_sdtm_results.md)
datasets, combining them into a multi-study example.

## Usage

``` r
generate_sample_csv(
  path,
  sdtm_categories = c("ALT", "AST", "CREAT", "VS_DIABP", "VS_HEIGHT", "VS_PULSE",
    "VS_SYSBP", "VS_TEMP", "VS_WEIGHT", "RS_OVRLRESP", "VS_WEIGHT_CAT")
)
```

## Arguments

- path:

  Directory where CSVs will be written. Created if it does not exist.

- sdtm_categories:

  Character vector of `parameter_category_2` values to include from the
  SDTM sample. Pass `NULL` to include all.

## Value

A character vector of the file paths written (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
generate_sample_csv(tempdir())
} # }
```
