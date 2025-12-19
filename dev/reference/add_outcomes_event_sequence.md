# Add outcomes event sequence

add_outcomes_event_sequence() is an Add function that updates an object
by adding new values to new or empty fields. Specifically, this function
implements an algorithm to add outcomes event sequence. The function is
called for its side effects and does not return a value.

## Usage

``` r
add_outcomes_event_sequence(
  X_Ready4useDyad,
  inputs_ls,
  add_sensitivity_1L_lgl = FALSE,
  adjustment_1L_dbl = -2,
  iterations_int = 1:100L,
  k10_draws_fn = add_project_1_k10_draws,
  k10_method_1L_chr = c("Model", "Table"),
  k10_var_1L_chr = "k10",
  sensitivities_ls = make_sensitivities_ls(),
  suffix_1L_chr = character(0),
  tfmn_ls = make_class_tfmns(T),
  tx_prefix_1L_chr = "treatment",
  utilities_chr = c("CHU9D", "AQoL6D"),
  type_1L_chr = c("Model", "Project"),
  update_1L_int = integer(0)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- inputs_ls:

  Inputs (a list)

- add_sensitivity_1L_lgl:

  Add sensitivity (a logical vector of length one), Default: FALSE

- adjustment_1L_dbl:

  Adjustment (a double vector of length one), Default: -2

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- k10_draws_fn:

  K10 draws (a function), Default: add_project_1_k10_draws

- k10_method_1L_chr:

  K10 method (a character vector of length one), Default: c("Model",
  "Table")

- k10_var_1L_chr:

  K10 variable (a character vector of length one), Default: 'k10'

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- suffix_1L_chr:

  Suffix (a character vector of length one), Default: character(0)

- tfmn_ls:

  Transformation (a list), Default: make_class_tfmns(T)

- tx_prefix_1L_chr:

  Treatment prefix (a character vector of length one), Default:
  'treatment'

- utilities_chr:

  Utilities (a character vector), Default: c("CHU9D", "AQoL6D")

- type_1L_chr:

  Type (a character vector of length one), Default: c("Model",
  "Project")

- update_1L_int:

  Update (an integer vector of length one), Default: integer(0)

## Value

X (A dataset and data dictionary pair.)
