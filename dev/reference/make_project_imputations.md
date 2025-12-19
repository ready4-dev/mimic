# Make project imputations

make_project_imputations() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project imputations. The function returns Model data (a list).

## Usage

``` r
make_project_imputations(
  X_Ready4useDyad,
  Y_Ready4useDyad = ready4use::Ready4useDyad(),
  add_cumulatives_1L_lgl = FALSE,
  characteristics_chr = c("Diagnosis", "Employment"),
  date_vars_chr = "Date",
  extras_chr = character(0),
  filter_true_1L_chr = "FlexPsych",
  ignore_x_chr = character(0),
  ignore_y_chr = character(0),
  imputations_1L_int = 5,
  max_iterations_1L_int = 2,
  method_1L_chr = "rf",
  post_imputation_fn = identity,
  uid_var_1L_chr = "UID"
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

  Characteristics (a character vector), Default: c("Diagnosis",
  "Employment")

- date_vars_chr:

  Date variables (a character vector), Default: 'Date'

- extras_chr:

  Extras (a character vector), Default: character(0)

- filter_true_1L_chr:

  Filter true (a character vector of length one), Default: 'FlexPsych'

- ignore_x_chr:

  Ignore x (a character vector), Default: character(0)

- ignore_y_chr:

  Ignore y (a character vector), Default: character(0)

- imputations_1L_int:

  Imputations (an integer vector of length one), Default: 5

- max_iterations_1L_int:

  Maximum iterations (an integer vector of length one), Default: 2

- method_1L_chr:

  Method (a character vector of length one), Default: 'rf'

- post_imputation_fn:

  Post imputation (a function), Default: identity

- uid_var_1L_chr:

  Unique identifier variable (a character vector of length one),
  Default: 'UID'

## Value

Model data (a list)
