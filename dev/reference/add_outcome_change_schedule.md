# Add outcome change schedule

add_outcome_change_schedule() is an Add function that updates an object
by adding new values to new or empty fields. Specifically, this function
implements an algorithm to add outcome change schedule. The function is
called for its side effects and does not return a value.

## Usage

``` r
add_outcome_change_schedule(X_Ready4useDyad, step_dtm = lubridate::weeks(0))
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- step_dtm:

  Step (a date vector), Default: lubridate::weeks(0)

## Value

X (A dataset and data dictionary pair.)
