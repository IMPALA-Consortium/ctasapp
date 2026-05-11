# Load app configuration from a YAML file

Reads a YAML config file and merges with built-in defaults so that
missing keys fall back gracefully. When `path` is `NULL` the file is
resolved using a discovery chain:

1.  `ctasapp.yml` in the current working directory (i.e. the folder
    containing `app.R` when launched by Posit Connect or
    [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)).

2.  `system.file("ctasapp.yml", package = "ctasapp")` (the copy shipped
    with the installed package).

If neither is found, the built-in
[`default_config()`](https://IMPALA-Consortium.github.io/ctasapp/reference/default_config.md)
is returned.

## Usage

``` r
load_config(path = NULL)
```

## Arguments

- path:

  Path to a YAML file. When `NULL` (default), the discovery chain above
  is used.

## Value

A nested list with elements `colors`, `icons`, `features`, and
`embedded`.
