# Field Detail Module - Server

Renders a clickable list of parameters in the sidebar. On selection,
shows pill-tabbed score tables (regular + missingness), the timeseries
plot, and a raw data table for outlier sites. Auto-detects plot type
from `parameter_category_3`. Supports feature sub-selection via checkbox
group.

## Usage

``` r
mod_FieldDetail_server(
  id,
  rctv_measures,
  rctv_ctas_results,
  rctv_untransformed = shiny::reactiveVal(NULL),
  rctv_queries = shiny::reactiveVal(NULL),
  rctv_dataset_label = shiny::reactiveVal(NULL),
  rctv_studies = shiny::reactiveVal(NULL)
)
```

## Arguments

- id:

  Module namespace ID.

- rctv_measures:

  Reactive expression returning the measures data frame.

- rctv_ctas_results:

  Reactive expression returning the raw ctas results list.

- rctv_untransformed:

  Reactive expression returning the untransformed timeseries data frame
  (NULL for ctas sample data).

- rctv_queries:

  Reactive expression returning the query data frame (NULL when no
  queries are available).

- rctv_dataset_label:

  Reactive expression returning the dataset label string (e.g. "ctas
  sample", "SDTM sample", or a user filename).

- rctv_studies:

  Reactive expression returning a character vector of available study
  names, or NULL when data has no study column or only one study.
