# Predict digital pathway

predict_digital_pathway() is a Predict function that applies a model to
make predictions. Specifically, this function implements an algorithm to
predict digital pathway. The function is called for its side effects and
does not return a value.

## Usage

``` r
predict_digital_pathway(
  inputs_ls,
  add_logic_fn = add_project_offset_logic,
  arm_1L_chr = "Intervention",
  base_for_rates_int = c(1000L, 1L, 1L),
  draws_tb = NULL,
  extra_draws_fn = add_draws_from_pool,
  iterations_int = 1:100L,
  horizon_dtm = lubridate::years(1),
  modifiable_chr = c("treatment_status", "Minutes", "k10", "AQoL6D", "CHU9D"),
  seed_1L_int = 2001L,
  sensitivities_ls = make_sensitivities_ls(),
  start_dtm = Sys.Date(),
  tfmn_ls = make_class_tfmns(),
  tx_duration_dtm = lubridate::weeks(12),
  utilities_chr = c("CHU9D", "AQoL6D"),
  utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr),
  variable_unit_1L_chr = "Minutes"
)
```

## Arguments

- inputs_ls:

  Inputs (a list)

- add_logic_fn:

  Add logic (a function), Default: add_project_offset_logic

- arm_1L_chr:

  Arm (a character vector of length one), Default: 'Intervention'

- base_for_rates_int:

  Base for rates (an integer vector), Default: c(1000L, 1L, 1L)

- draws_tb:

  Draws (a tibble), Default: NULL

- extra_draws_fn:

  Extra draws (a function), Default: add_draws_from_pool

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- horizon_dtm:

  Horizon (a date vector), Default: lubridate::years(1)

- modifiable_chr:

  Modifiable (a character vector), Default: c("treatment_status",
  "Minutes", "k10", "AQoL6D", "CHU9D")

- seed_1L_int:

  Seed (an integer vector of length one), Default: 2001

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- start_dtm:

  Start (a date vector), Default: Sys.Date()

- tfmn_ls:

  Transformation (a list), Default: make_class_tfmns()

- tx_duration_dtm:

  Treatment duration (a date vector), Default: lubridate::weeks(12)

- utilities_chr:

  Utilities (a character vector), Default: c("CHU9D", "AQoL6D")

- utility_fns_ls:

  Utility functions (a list), Default: make_utility_fns_ls(utilities_chr
  = utilities_chr)

- variable_unit_1L_chr:

  Variable unit (a character vector of length one), Default: 'Minutes'

## Value

X (A dataset and data dictionary pair.)
