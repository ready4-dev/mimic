# Add imputed data

add_imputed_data() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add imputed data. The function is called for
its side effects and does not return a value.

## Usage

``` r
add_imputed_data(
  X_Ready4useDyad,
  Y_Ready4useDyad = ready4use::Ready4useDyad(),
  add_cumulatives_1L_lgl = FALSE,
  characteristics_chr = c("platform", "clinic_type", "gender", "employment_status",
    "clinic_state", "clinic_postcode", "role_type", "treatment_stage"),
  extras_chr = character(0),
  ignore_x_chr = character(0),
  ignore_y_chr = character(0),
  impute_age_1L_lgl = FALSE,
  method_1L_chr = "rf",
  treatment_status_1L_int = 0L,
  x_is_z_1L_lgl = FALSE
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- Y_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()

- add_cumulatives_1L_lgl:

  Add cumulatives (a logical vector of length one), Default: FALSE

- characteristics_chr:

  Characteristics (a character vector), Default: c("platform",
  "clinic_type", "gender", "employment_status", "clinic_state",
  "clinic_postcode", "role_type", "treatment_stage")

- extras_chr:

  Extras (a character vector), Default: character(0)

- ignore_x_chr:

  Ignore x (a character vector), Default: character(0)

- ignore_y_chr:

  Ignore y (a character vector), Default: character(0)

- impute_age_1L_lgl:

  Impute age (a logical vector of length one), Default: FALSE

- method_1L_chr:

  Method (a character vector of length one), Default: 'rf'

- treatment_status_1L_int:

  Treatment status (an integer vector of length one), Default: 0

- x_is_z_1L_lgl:

  X is z (a logical vector of length one), Default: FALSE

## Value

Z (A dataset and data dictionary pair.)
