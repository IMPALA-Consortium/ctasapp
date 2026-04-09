# Contributor Guidelines

## Branches

The core branches used in this repository are:

- `main`: Contains the production version of the package (protected).
- `{issue#}`: Contains the code for a specific issue, named with the
  issue number as a prefix.

## Development Process

1.  Describe the development step in a GitHub issue.
2.  Create a new branch from `main` named with the issue number as a
    prefix (e.g. `42-add-pca-tab`).
3.  Use **test-driven development (TDD)**: write the test first, then
    write the code to pass the test.
4.  All GitHub Actions must pass before merging the branch to `main`.

## Code Style

- Code is formatted with [{styler}](https://styler.r-lib.org/) before
  each release.
- You can trigger [styler](https://github.com/r-lib/styler) on a PR by
  commenting `/style` (available to repository members).

## Test Coverage

CI enforces **100 % line coverage** (`covr::percent_coverage() == 100`).
Every line of R code must either be exercised by a test **or**
explicitly excluded with a `# nocov` annotation.

### When `# nocov` is acceptable

| Category                                                            | Example                                           | Annotation style                                 |
|---------------------------------------------------------------------|---------------------------------------------------|--------------------------------------------------|
| Shiny reactive / render blocks that depend on `input$` or `session` | `output$plot <- renderPlot({ ... })`              | `# nocov start` / `# nocov end` around the block |
| Error-handling UI paths inside `observeEvent` / `withProgress`      | `shiny::showNotification(...)` after a `tryCatch` | Same block style                                 |
| Defensive guards that can only trigger inside a running app         | `if (length(sites) > 24)` cap                     | Inline `# nocov`                                 |

### When `# nocov` is **not** acceptable

- Pure (non-Shiny) utility or plotting functions — write a test instead.
- Branches that can be reached by constructing the right data frame in a
  test.
- New exported functions — every exported function must have at least
  one test.

When adding `# nocov`, keep the excluded region as small as possible and
prefer inline `# nocov` over start/end blocks for single lines.

## Documentation

- All exported functions must have
  [roxygen2](https://roxygen2.r-lib.org/) documentation.
- You can regenerate documentation on a PR by commenting `/document`
  (available to repository members).
- Roxygen import tags are centralized in `R/ctasapp-pkg.R`.
