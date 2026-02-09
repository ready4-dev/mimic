# Predict project 2 pathway

predict_project_2_pathway() is a Predict function that applies a model
to make predictions. Specifically, this function implements an algorithm
to predict project 2 pathway. The function is called for its side
effects and does not return a value.

## Usage

``` r
predict_project_2_pathway(
  inputs_ls = NULL,
  add_logic_fn = identity,
  arm_1L_chr,
  arms_for_intervention_costs_chr,
  arms_for_offsets_chr = character(0),
  arms_for_non_helpseeking_chr = character(0),
  arms_for_iar_adjustment_chr = character(0),
  batch_1L_int = integer(0),
  derive_extras_ls = list(),
  draws_tb = NULL,
  extra_draws_fn = NULL,
  horizon_dtm = lubridate::years(1),
  iterations_int = 1:100L,
  modifiable_chr = make_project_2_vars("modify"),
  seed_1L_int = 2001L,
  sensitivities_ls = make_project_2_sensitivities_ls(),
  start_dtm = Sys.Date(),
  tfmn_ls = make_class_tfmns(),
  tx_duration_dtm = lubridate::weeks(12),
  treatment_ls = NULL,
  utilities_chr = c("AQoL8D", "EQ5D", "EQ5DM2", "SF6D", "SF6DM2"),
  utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr),
  X_MimicConfiguration = MimicConfiguration()
)
```

## Arguments

- inputs_ls:

  Inputs (a list), Default: NULL

- add_logic_fn:

  Add logic (a function), Default: identity

- arm_1L_chr:

  Arm (a character vector of length one)

- arms_for_intervention_costs_chr:

  Arms for intervention costs (a character vector)

- arms_for_offsets_chr:

  Arms for offsets (a character vector), Default: character(0)

- arms_for_non_helpseeking_chr:

  Arms for non helpseeking (a character vector), Default: character(0)

- arms_for_iar_adjustment_chr:

  Arms for Initial Assessment andeferral adjustment (a character
  vector), Default: character(0)

- batch_1L_int:

  Batch (an integer vector of length one), Default: integer(0)

- derive_extras_ls:

  Derive extras (a list), Default: list()

- draws_tb:

  Draws (a tibble), Default: NULL

- extra_draws_fn:

  Extra draws (a function), Default: NULL

- horizon_dtm:

  Horizon (a date vector), Default: lubridate::years(1)

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- modifiable_chr:

  Modifiable (a character vector), Default:
  make_project_2_vars("modify")

- seed_1L_int:

  Seed (an integer vector of length one), Default: 2001

- sensitivities_ls:

  Sensitivities (a list), Default: make_project_2_sensitivities_ls()

- start_dtm:

  Start (a date vector), Default: Sys.Date()

- tfmn_ls:

  Transformation (a list), Default: make_class_tfmns()

- tx_duration_dtm:

  Treatment duration (a date vector), Default: lubridate::weeks(12)

- treatment_ls:

  Treatment (a list), Default: NULL

- utilities_chr:

  Utilities (a character vector), Default: c("AQoL8D", "EQ5D", "EQ5DM2",
  "SF6D", "SF6DM2")

- utility_fns_ls:

  Utility functions (a list), Default: make_utility_fns_ls(utilities_chr
  = utilities_chr)

- X_MimicConfiguration:

  PARAM_DESCRIPTION, Default: MimicConfiguration()

## Value

Population ls\$X (A dataset and data dictionary pair.)
