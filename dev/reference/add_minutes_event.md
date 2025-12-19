# Add minutes event

add_minutes_event() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add minutes event. The function is called for
its side effects and does not return a value.

## Usage

``` r
add_minutes_event(
  X_Ready4useDyad,
  add_dependency_1L_lgl = T,
  minutes_mdl = NULL,
  iterations_int = 1:100L,
  fraction_1L_dbl = numeric(0),
  var_1L_chr = "Minutes"
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- add_dependency_1L_lgl:

  Add dependency (a logical vector of length one), Default: T

- minutes_mdl:

  Minutes (a model), Default: NULL

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- fraction_1L_dbl:

  Fraction (a double vector of length one), Default: numeric(0)

- var_1L_chr:

  Variable (a character vector of length one), Default: 'Minutes'

## Value

X (A dataset and data dictionary pair.)
