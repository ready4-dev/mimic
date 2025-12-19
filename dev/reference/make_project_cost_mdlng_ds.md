# Make project cost modelling dataset

make_project_cost_mdlng_ds() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project cost modelling dataset. The function is called for its side
effects and does not return a value.

## Usage

``` r
make_project_cost_mdlng_ds(
  W_Ready4useDyad,
  X_Ready4useDyad,
  transform_gender_1L_lgl = T
)
```

## Arguments

- W_Ready4useDyad:

  PARAM_DESCRIPTION

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- transform_gender_1L_lgl:

  Transform gender (a logical vector of length one), Default: T

## Value

Z (A dataset and data dictionary pair.)
