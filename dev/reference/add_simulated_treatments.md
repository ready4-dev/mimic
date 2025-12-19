# Add simulated treatments

add_simulated_treatments() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add simulated treatments. The function is
called for its side effects and does not return a value.

## Usage

``` r
add_simulated_treatments(
  treatment_mdls_ls,
  Y_Ready4useDyad,
  bl_week_1L_dbl = 0,
  change_var_1L_chr = "treatment_change",
  iterations_int = 1:100L,
  status_var_1L_chr = "treatment_status",
  tidy_1L_lgl = FALSE
)
```

## Arguments

- treatment_mdls_ls:

  Treatment models (a list)

- Y_Ready4useDyad:

  PARAM_DESCRIPTION

- bl_week_1L_dbl:

  Baseline week (a double vector of length one), Default: 0

- change_var_1L_chr:

  Change variable (a character vector of length one), Default:
  'treatment_change'

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- status_var_1L_chr:

  Status variable (a character vector of length one), Default:
  'treatment_status'

- tidy_1L_lgl:

  Tidy (a logical vector of length one), Default: FALSE

## Value

Y (A dataset and data dictionary pair.)
