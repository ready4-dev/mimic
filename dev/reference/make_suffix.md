# Make suffix

make_suffix() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make suffix. The
function returns Suffix (a character vector of length one).

## Usage

``` r
make_suffix(
  X_Ready4useDyad,
  adjustment_1L_dbl = 0,
  follow_up_1L_int = integer(0),
  sensitivities_ls = make_sensitivities_ls(),
  type_1L_chr = c("Model", "Function", "Project"),
  update_1L_int = integer(0)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- adjustment_1L_dbl:

  Adjustment (a double vector of length one), Default: 0

- follow_up_1L_int:

  Follow up (an integer vector of length one), Default: integer(0)

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- type_1L_chr:

  Type (a character vector of length one), Default: c("Model",
  "Function", "Project")

- update_1L_int:

  Update (an integer vector of length one), Default: integer(0)

## Value

Suffix (a character vector of length one)
