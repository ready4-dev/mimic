# Predict with sim

predict_with_sim() is a Predict function that applies a model to make
predictions. Specifically, this function implements an algorithm to
predict with sim. The function returns Output (an output object of
multiple potential types).

## Usage

``` r
predict_with_sim(
  inputs_ls = NULL,
  arms_chr = c("Intervention", "Comparator"),
  comparator_fn = predict_comparator_pathway,
  draws_tb = NULL,
  drop_missing_1L_lgl = FALSE,
  drop_suffix_1L_chr = character(0),
  extra_draws_fn = NULL,
  intervention_fn = predict_digital_pathway,
  iterations_ls = make_batches(5, of_1L_int = 20),
  horizon_dtm = lubridate::years(1),
  modifiable_chr = c("treatment_status", "Minutes", "k10", "AQoL6D", "CHU9D"),
  prior_batches_1L_int = 0,
  purge_1L_lgl = TRUE,
  seed_1L_int = 2001L,
  sensitivities_ls = make_sensitivities_ls(),
  synthesis_fn = make_project_results_synthesis,
  start_dtm = Sys.Date(),
  tfmn_ls = make_class_tfmns(),
  type_1L_chr = c("D", "AB", "C", "NULL"),
  unlink_1L_lgl = FALSE,
  utilities_chr = c("AQoL6D", "CHU9D"),
  write_to_1L_chr = character(0),
  X_MimicConfiguration = MimicConfiguration(),
  ...
)
```

## Arguments

- inputs_ls:

  Inputs (a list), Default: NULL

- arms_chr:

  Arms (a character vector), Default: c("Intervention", "Comparator")

- comparator_fn:

  Comparator (a function), Default: predict_comparator_pathway

- draws_tb:

  Draws (a tibble), Default: NULL

- drop_missing_1L_lgl:

  Drop missing (a logical vector of length one), Default: FALSE

- drop_suffix_1L_chr:

  Drop suffix (a character vector of length one), Default: character(0)

- extra_draws_fn:

  Extra draws (a function), Default: NULL

- intervention_fn:

  Intervention (a function), Default: predict_digital_pathway

- iterations_ls:

  Iterations (a list), Default: make_batches(5, of_1L_int = 20)

- horizon_dtm:

  Horizon (a date vector), Default: lubridate::years(1)

- modifiable_chr:

  Modifiable (a character vector), Default: c("treatment_status",
  "Minutes", "k10", "AQoL6D", "CHU9D")

- prior_batches_1L_int:

  Prior batches (an integer vector of length one), Default: 0

- purge_1L_lgl:

  Purge (a logical vector of length one), Default: TRUE

- seed_1L_int:

  Seed (an integer vector of length one), Default: 2001

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- synthesis_fn:

  Synthesis (a function), Default: make_project_results_synthesis

- start_dtm:

  Start (a date vector), Default: Sys.Date()

- tfmn_ls:

  Transformation (a list), Default: make_class_tfmns()

- type_1L_chr:

  Type (a character vector of length one), Default: c("D", "AB", "C",
  "NULL")

- unlink_1L_lgl:

  Unlink (a logical vector of length one), Default: FALSE

- utilities_chr:

  Utilities (a character vector), Default: c("AQoL6D", "CHU9D")

- write_to_1L_chr:

  Write to (a character vector of length one), Default: character(0)

- X_MimicConfiguration:

  PARAM_DESCRIPTION, Default: MimicConfiguration()

- ...:

  Additional arguments

## Value

Output (an output object of multiple potential types)
