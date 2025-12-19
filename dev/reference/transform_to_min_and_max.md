# Transform to minimum and maximum

transform_to_min_and_max() is a Transform function that edits an object
in such a way that core object attributes - e.g. shape, dimensions,
elements, type - are altered. Specifically, this function implements an
algorithm to transform to minimum and maximum. The function is called
for its side effects and does not return a value.

## Usage

``` r
transform_to_min_and_max(
  X_Ready4useDyad,
  vars_chr,
  max_1L_dbl = 0.999999,
  min_1L_dbl = 0.000001
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- vars_chr:

  Variables (a character vector)

- max_1L_dbl:

  Maximum (a double vector of length one), Default: 0.999999

- min_1L_dbl:

  Minimum (a double vector of length one), Default: 0.000001

## Value

Y (A dataset and data dictionary pair.)
