# Add minutes

add_minutes() is an Add function that updates an object by adding new
values to new or empty fields. Specifically, this function implements an
algorithm to add minutes. The function is called for its side effects
and does not return a value.

## Usage

``` r
add_minutes(
  X_Ready4useDyad = ready4use::Ready4useDyad(),
  Y_Ready4useDyad,
  end_dtm = NULL,
  period_dtm = lubridate::years(1),
  start_at_1L_int = -2L,
  weeks_dbl = c(14, 53)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()

- Y_Ready4useDyad:

  PARAM_DESCRIPTION

- end_dtm:

  End (a date vector), Default: NULL

- period_dtm:

  Period (a date vector), Default: lubridate::years(1)

- start_at_1L_int:

  Start at (an integer vector of length one), Default: -2L

- weeks_dbl:

  Weeks (a double vector), Default: c(14, 53)

## Value

X (A dataset and data dictionary pair.)
