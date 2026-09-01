# ctasapp 0.4.0.900
* control linkage between score tables, plots and data tables with radio button
* clear missingness outlier indication
* clear failure and bug reporting
* new Protocol Deviations tab in Field Detail (site-level linkage) with
  matching upload/embedded/sample workflow (`simulate_pd_data()`,
  `validate_upload_pd()`, `pd` element in sample data and embedded config)

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

