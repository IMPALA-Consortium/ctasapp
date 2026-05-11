# Data Input Module - UI

Provides a dropdown to select sample datasets or upload custom files.
Upload mode is selected by default. When uploading, 2 mandatory file
inputs (results, input) and 2 optional file inputs (untransformed,
queries) are shown with collapsible format documentation.

## Usage

``` r
mod_DataInput_ui(id)
```

## Arguments

- id:

  Module namespace ID.

## Details

When the `embedded:` block is set in `ctasapp.yml` (see
[`load_config()`](https://IMPALA-Consortium.github.io/ctasapp/reference/load_config.md)
/
[`get_embedded_paths()`](https://IMPALA-Consortium.github.io/ctasapp/reference/get_embedded_paths.md)),
an additional "Embedded data set" entry is added to the dropdown that
loads the configured files through the same pipeline as uploads.
