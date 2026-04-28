# Conditional app logger

Prints a message to the console when the `ctasapp.verbose` option is
`TRUE` (the default). Set `options(ctasapp.verbose = FALSE)` or pass
`verbose = FALSE` to
[`run_ctas_app()`](https://IMPALA-Consortium.github.io/ctasapp/reference/run_ctas_app.md)
to silence all log output.

## Usage

``` r
ctas_log(...)
```

## Arguments

- ...:

  Passed to [`base::message()`](https://rdrr.io/r/base/message.html).
