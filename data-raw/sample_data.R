library(ctas)

pkgload::load_all(".", export_all = FALSE)

data("ctas_data", package = "ctas")

feats <- paste(
  c(
    "autocorr",
    "average",
    "own_site_simil_score",
    "sd",
    "unique_value_count_relative",
    "lof",
    "range"
  ),
  collapse = ";"
)

ctas_results <- process_a_study(
  data = ctas_data$data,
  subjects = ctas_data$subjects,
  parameters = ctas_data$parameters,
  custom_timeseries = ctas_data$custom_timeseries,
  custom_reference_groups = ctas_data$custom_reference_groups,
  default_timeseries_features_to_calculate = feats,
  default_minimum_timepoints_per_series = 3,
  default_minimum_subjects_per_series = 3,
  default_max_share_missing_timepoints_per_series = 0.5,
  default_generate_change_from_baseline = FALSE,
  autogenerate_timeseries = TRUE
)

sample_ctas_data <- ctas_data[c("data", "subjects", "parameters")]
sample_ctas_data$queries <- simulate_query_data(sample_ctas_data, seed = 456)
sample_ctas_results <- ctas_results

usethis::use_data(sample_ctas_data, overwrite = TRUE)
usethis::use_data(sample_ctas_results, overwrite = TRUE)
