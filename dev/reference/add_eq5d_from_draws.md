# Add EQ5D from draws

add_eq5d_from_draws() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add eq5d from draws. The function is called
for its side effects and does not return a value.

## Usage

``` r
add_eq5d_from_draws(
  X_Ready4useDyad,
  correspondences_r3 = ready4show::ready4show_correspondences(),
  prefix_1L_chr = "ParamEQ5DBeta",
  value_with_fn = add_eq5d_from_k10,
  var_1L_chr = "EQ5D"
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- correspondences_r3:

  Correspondences (a ready4 submodule), Default:
  ready4show::ready4show_correspondences()

- prefix_1L_chr:

  Prefix (a character vector of length one), Default: 'ParamEQ5DBeta'

- value_with_fn:

  Value with (a function), Default: add_eq5d_from_k10

- var_1L_chr:

  Variable (a character vector of length one), Default: 'EQ5D'

## Value

X (A dataset and data dictionary pair.)
