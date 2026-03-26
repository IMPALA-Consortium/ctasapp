library(pharmaversesdtm)
library(ctas)

pkgload::load_all(".", export_all = FALSE)

data("dm", package = "pharmaversesdtm")
data("lb", package = "pharmaversesdtm")
data("vs", package = "pharmaversesdtm")
data("rs_onco", package = "pharmaversesdtm")

labs <- Input_Labs(dm, lb)
vs_input <- Input_VS(dm, vs)
rs <- Input_RS(dm, rs_onco)
bmi <- Input_BMI(dm, vs)

sample_sdtm_data <- combine_ctas_input(labs, vs_input, rs, bmi)

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

sample_sdtm_results <- process_a_study(
  data = sample_sdtm_data$data,
  subjects = sample_sdtm_data$subjects,
  parameters = sample_sdtm_data$parameters,
  custom_timeseries = sample_sdtm_data$custom_timeseries,
  custom_reference_groups = sample_sdtm_data$custom_reference_groups,
  default_timeseries_features_to_calculate = feats,
  default_minimum_timepoints_per_series = 1,
  default_minimum_subjects_per_series = 3,
  default_max_share_missing_timepoints_per_series = 0.5,
  default_generate_change_from_baseline = FALSE,
  autogenerate_timeseries = TRUE
)

usethis::use_data(sample_sdtm_data, overwrite = TRUE)
usethis::use_data(sample_sdtm_results, overwrite = TRUE)
