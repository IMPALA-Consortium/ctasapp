# Run the ctas Shiny App

Launches the interactive ctas visualization application.

## Usage

``` r
run_ctas_app(config = NULL, ...)
```

## Arguments

- config:

  Path to a YAML configuration file. When `NULL` (default), uses the
  config shipped with the package. See
  [`load_config()`](https://IMPALA-Consortium.github.io/ctasapp/reference/load_config.md)
  for the expected structure.

- ...:

  Additional arguments passed to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).
