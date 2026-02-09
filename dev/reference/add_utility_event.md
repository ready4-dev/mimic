# Add utility event

add_utility_event() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add utility event. The function is called for
its side effects and does not return a value.

## Usage

``` r
add_utility_event(
  X_Ready4useDyad,
  add_qalys_1L_lgl = FALSE,
  add_sensitivity_1L_lgl = FALSE,
  adjustment_1L_dbl = 0,
  follow_up_1L_int = integer(0),
  iterations_int = 1:100L,
  maintain_for_1L_int = 0L,
  models_ls = NULL,
  rewind_chr = character(0),
  sensitivities_ls = make_sensitivities_ls(),
  simulate_1L_lgl = TRUE,
  tfmn_ls = NULL,
  tidy_1L_lgl = TRUE,
  tidy_cols_1L_lgl = FALSE,
  update_1L_int = integer(0),
  utilities_chr = c("CHU9D", "AQoL6D"),
  utility_fns_ls = make_utility_fns_ls(utilities_chr = utilities_chr),
  type_1L_chr = c("Model", "Function", "Project"),
  what_1L_chr = c("old", "new")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- add_qalys_1L_lgl:

  Add Quality Adjusted Life Years (a logical vector of length one),
  Default: FALSE

- add_sensitivity_1L_lgl:

  Add sensitivity (a logical vector of length one), Default: FALSE

- adjustment_1L_dbl:

  Adjustment (a double vector of length one), Default: 0

- follow_up_1L_int:

  Follow up (an integer vector of length one), Default: integer(0)

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- maintain_for_1L_int:

  Maintain for (an integer vector of length one), Default: 0

- models_ls:

  Models (a list), Default: NULL

- rewind_chr:

  Rewind (a character vector), Default: character(0)

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- simulate_1L_lgl:

  Simulate (a logical vector of length one), Default: TRUE

- tfmn_ls:

  Transformation (a list), Default: NULL

- tidy_1L_lgl:

  Tidy (a logical vector of length one), Default: TRUE

- tidy_cols_1L_lgl:

  Tidy columns (a logical vector of length one), Default: FALSE

- update_1L_int:

  Update (an integer vector of length one), Default: integer(0)

- utilities_chr:

  Utilities (a character vector), Default: c("CHU9D", "AQoL6D")

- utility_fns_ls:

  Utility functions (a list), Default: make_utility_fns_ls(utilities_chr
  = utilities_chr)

- type_1L_chr:

  Type (a character vector of length one), Default: c("Model",
  "Function", "Project")

- what_1L_chr:

  What (a character vector of length one), Default: c("old", "new")

## Value

X (A dataset and data dictionary pair.)
