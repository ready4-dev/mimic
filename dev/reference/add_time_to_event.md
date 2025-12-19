# Add time to event

add_time_to_event() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add time to event. The function is called for
its side effects and does not return a value.

## Usage

``` r
add_time_to_event(
  X_Ready4useDyad,
  event_1L_chr,
  schedule_args_ls = list(),
  schedule_fn = NULL,
  step_dtm = lubridate::days(0)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- event_1L_chr:

  Event (a character vector of length one)

- schedule_args_ls:

  Schedule arguments (a list), Default: list()

- schedule_fn:

  Schedule (a function), Default: NULL

- step_dtm:

  Step (a date vector), Default: lubridate::days(0)

## Value

X (A dataset and data dictionary pair.)
