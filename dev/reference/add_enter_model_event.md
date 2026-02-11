# Add enter model event

add_enter_model_event() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add enter model event. The function is called
for its side effects and does not return a value.

## Usage

``` r
add_enter_model_event(
  X_Ready4useDyad,
  arm_1L_chr,
  draws_tb,
  horizon_dtm = lubridate::years(1),
  default_args_ls = list(),
  default_fn = NULL,
  derive_fn_ls = NULL,
  iterations_int = 1:100L,
  modifiable_chr = character(0),
  start_dtm = Sys.Date(),
  tidy_cols_1L_lgl = FALSE,
  tfmn_ls = NULL,
  tx_duration_dtm = lubridate::weeks(12),
  tx_prefix_1L_chr = "treatment"
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- arm_1L_chr:

  Arm (a character vector of length one)

- draws_tb:

  Draws (a tibble)

- horizon_dtm:

  Horizon (a date vector), Default: lubridate::years(1)

- default_args_ls:

  Default arguments (a list), Default: list()

- default_fn:

  Default (a function), Default: NULL

- derive_fn_ls:

  Derive (a list of functions), Default: NULL

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- modifiable_chr:

  Modifiable (a character vector), Default: character(0)

- start_dtm:

  Start (a date vector), Default: Sys.Date()

- tidy_cols_1L_lgl:

  Tidy columns (a logical vector of length one), Default: FALSE

- tfmn_ls:

  Transformation (a list), Default: NULL

- tx_duration_dtm:

  Treatment duration (a date vector), Default: lubridate::weeks(12)

- tx_prefix_1L_chr:

  Treatment prefix (a character vector of length one), Default:
  'treatment'

## Value

X (A dataset and data dictionary pair.)
