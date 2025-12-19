# Add Short Form - Six Dimension from draws

add_sf6d_from_draws() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add short form - six dimension from draws.
The function is called for its side effects and does not return a value.

## Usage

``` r
add_sf6d_from_draws(
  X_Ready4useDyad,
  correspondences_r3 = ready4show::ready4show_correspondences(),
  female_values_chr = c("Female", "female", "F", "f", "FEMALE"),
  male_values_chr = c("Male", "male", "M", "m", "MALE"),
  prefix_1L_chr = "ParamSF6DBeta",
  var_1L_chr = "SF6D"
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- correspondences_r3:

  Correspondences (a ready4 submodule), Default:
  ready4show::ready4show_correspondences()

- female_values_chr:

  Female values (a character vector), Default: c("Female", "female",
  "F", "f", "FEMALE")

- male_values_chr:

  Male values (a character vector), Default: c("Male", "male", "M", "m",
  "MALE")

- prefix_1L_chr:

  Prefix (a character vector of length one), Default: 'ParamSF6DBeta'

- var_1L_chr:

  Variable (a character vector of length one), Default: 'SF6D'

## Value

X (A dataset and data dictionary pair.)
