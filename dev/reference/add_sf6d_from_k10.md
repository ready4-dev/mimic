# Add Short Form - Six Dimension from K10

add_sf6d_from_k10() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add short form - six dimension from k10. The
function returns Data (an output object of multiple potential types).

## Usage

``` r
add_sf6d_from_k10(
  data_xx,
  correspondences_r3 = ready4show::ready4show_correspondences(),
  beta_female_moderate_1L_dbl = -0.059,
  beta_female_high_1L_dbl = -0.124,
  beta_male_moderate_1L_dbl = -0.055,
  beta_male_high_1L_dbl = -0.123,
  beta_constant_1L_dbl = 0.805,
  female_values_chr = c("Female", "female", "F", "f", "FEMALE"),
  male_values_chr = c("Male", "male", "M", "m", "MALE"),
  prefix_1L_chr = "ParamSF6DBeta",
  var_1L_chr = "SF6D",
  source_1L_chr = c("10.1016/j.jval.2024.12.002", "10.1192/bjp.bp.113.136036"),
  tidy_cols_1L_lgl = FALSE,
  type_1L_chr = c("internal", "external")
)
```

## Arguments

- data_xx:

  Data (an output object of multiple potential types)

- correspondences_r3:

  Correspondences (a ready4 submodule), Default:
  ready4show::ready4show_correspondences()

- beta_female_moderate_1L_dbl:

  Beta female moderate (a double vector of length one), Default: -0.059

- beta_female_high_1L_dbl:

  Beta female high (a double vector of length one), Default: -0.124

- beta_male_moderate_1L_dbl:

  Beta male moderate (a double vector of length one), Default: -0.055

- beta_male_high_1L_dbl:

  Beta male high (a double vector of length one), Default: -0.123

- beta_constant_1L_dbl:

  Beta constant (a double vector of length one), Default: 0.805

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

- source_1L_chr:

  Source (a character vector of length one), Default:
  c("10.1016/j.jval.2024.12.002", "10.1192/bjp.bp.113.136036")

- tidy_cols_1L_lgl:

  Tidy columns (a logical vector of length one), Default: FALSE

- type_1L_chr:

  Type (a character vector of length one), Default: c("internal",
  "external")

## Value

Data (an output object of multiple potential types)
