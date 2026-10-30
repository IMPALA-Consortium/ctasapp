# ctasapp 0.4.0.900
* control linkage between score tables, plots and data tables with radio button
* clear missingness outlier indication
* clear failure and bug reporting
* bug icons reflect CTAS scores: purple when only missingness parameters are
  scored, grey when no parameter of the field is scored
* fields without any CTAS scores show a site-only table in Regular Scores
  that drives the plot and source data
* categorical fields with a single timepoint per subject are plotted as bar
  charts automatically, no `parameter_category_3 = "bar"` required
* new Protocol Deviations tab in Field Detail (site-level linkage) with
  matching upload/embedded/sample workflow (`validate_upload_pd()`, `pd`
  element in sample data and embedded config)
* sample data is now generated with gsm.ctas: `sample_data` and
  `sample_results` replace `sample_ctas_*` and `sample_sdtm_*`, the data
  source dropdown offers a single "Sample data (gsm.ctas)" option
* gsm.ctas upload files are shipped as parquet in `inst/extdata` (~2 MB);
  `generate_sample_csv()` copies them as CSV or parquet (`format` argument,
  `sdtm_categories` argument removed)
* `arrow` moved from Suggests to Imports, parquet uploads always work
* removed data-generation helpers now provided by gsm.ctas: `Input_Labs()`,
  `Input_VS()`, `Input_RS()`, `Input_BMI()`, `combine_ctas_input()`,
  `inject_missingness()`, `simulate_query_data()`, `simulate_pd_data()`,
  `normalize_by_range()`, `ratio_missing_over_time()`,
  `recalculate_timepoint_rank()`, `encode_categorical()`,
  `arrange_timepoints()`

# ctasapp 0.4.0
* standard config renamed to ctasapp.yml
* ctasapp.yml allows embedded datasets
* by default will check parent directory for ctasapp.yml
* bugfix ensure additional columns in query and untransformed data sets pass through dashboard
* bugfix untranformed data not showing correct values when data set load contains multiple studies

# ctasapp 0.3.0
* support multistudy data sets
* fix sites to be displayed across all fields using site filter
* better organisation of fields in left navigation panel, sorting, scrolling
* replace debug statements with optional verbose logging

# ctasapp 0.2.0

* optimize loading of larger data set and avoid memory issues (#11)
* replace packaged csv files with `generate_sample_csv()` 
* limit plot rendering to sites visible in site summary table
* various UI improvements
* various bug fixes
* Added `# nocov` policy to README and Contributor Guidelines to adapt code coverage requirements.

# ctasapp 0.1.0

* Initial release.

