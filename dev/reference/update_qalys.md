# Update Quality Adjusted Life Years

update_qalys() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update quality adjusted life years. The
function is called for its side effects and does not return a value.

## Usage

``` r
update_qalys(
  X_Ready4useDyad,
  add_sensitivity_1L_lgl = FALSE,
  adjustment_1L_dbl = 0,
  follow_up_1L_int = integer(0),
  maintain_for_1L_int = 0,
  sensitivities_ls = make_sensitivities_ls(),
  tidy_1L_lgl = FALSE,
  utilities_chr = c("CHU9D", "AQoL6D")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- add_sensitivity_1L_lgl:

  Add sensitivity (a logical vector of length one), Default: FALSE

- adjustment_1L_dbl:

  Adjustment (a double vector of length one), Default: 0

- follow_up_1L_int:

  Follow up (an integer vector of length one), Default: integer(0)

- maintain_for_1L_int:

  Maintain for (an integer vector of length one), Default: 0

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- tidy_1L_lgl:

  Tidy (a logical vector of length one), Default: FALSE

- utilities_chr:

  Utilities (a character vector), Default: c("CHU9D", "AQoL6D")

## Value

X (A dataset and data dictionary pair.)
