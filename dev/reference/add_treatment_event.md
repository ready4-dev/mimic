# Add treatment event

add_treatment_event() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add treatment event. The function is called
for its side effects and does not return a value.

## Usage

``` r
add_treatment_event(
  X_Ready4useDyad,
  tx_models_ls,
  adjustment_1L_dbl = -2,
  bl_week_1L_dbl = 0,
  iterations_int = 1:100L,
  measurement_1L_int = integer(0),
  prefix_1L_chr = "treatment",
  tx_duration_dtm = lubridate::weeks(12)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- tx_models_ls:

  Treatment models (a list)

- adjustment_1L_dbl:

  Adjustment (a double vector of length one), Default: -2

- bl_week_1L_dbl:

  Baseline week (a double vector of length one), Default: 0

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- measurement_1L_int:

  Measurement (an integer vector of length one), Default: integer(0)

- prefix_1L_chr:

  Prefix (a character vector of length one), Default: 'treatment'

- tx_duration_dtm:

  Treatment duration (a date vector), Default: lubridate::weeks(12)

## Value

X (A dataset and data dictionary pair.)
