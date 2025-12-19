# Add iteration values set

add_iteration_values_set() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add iteration values set. The function is
called for its side effects and does not return a value.

## Usage

``` r
add_iteration_values_set(
  X_Ready4useDyad,
  value_with_fn,
  value_with_args_ls = NULL,
  tidy_cols_1L_lgl = TRUE
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- value_with_fn:

  Value with (a function)

- value_with_args_ls:

  Value with arguments (a list), Default: NULL

- tidy_cols_1L_lgl:

  Tidy columns (a logical vector of length one), Default: TRUE

## Value

X (A dataset and data dictionary pair.)
