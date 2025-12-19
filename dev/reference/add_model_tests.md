# Add model tests

add_model_tests() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add model tests. The function returns
Regressions (a list).

## Usage

``` r
add_model_tests(
  model_data_ls,
  regressions_ls,
  what_1L_chr,
  colour_1L_chr = character(0),
  colours_chr = character(0),
  imputed_1L_lgl = T,
  iterations_1L_int = 100,
  join_with_chr = character(0),
  max_1L_dbl = numeric(0),
  min_1L_dbl = numeric(0),
  model_1L_int = integer(0),
  plot_tfmn_fn = identity,
  summary_1L_lgl = FALSE,
  tfmn_fn = identity,
  type_1L_chr = c("models", "candidates"),
  uid_1L_chr = "UID",
  use_1L_chr = character(0),
  var_1L_chr = character(0),
  x_label_1L_chr = NA_character_
)
```

## Arguments

- model_data_ls:

  Model data (a list)

- regressions_ls:

  Regressions (a list)

- what_1L_chr:

  What (a character vector of length one)

- colour_1L_chr:

  Colour (a character vector of length one), Default: character(0)

- colours_chr:

  Colours (a character vector), Default: character(0)

- imputed_1L_lgl:

  Imputed (a logical vector of length one), Default: T

- iterations_1L_int:

  Iterations (an integer vector of length one), Default: 100

- join_with_chr:

  Join with (a character vector), Default: character(0)

- max_1L_dbl:

  Maximum (a double vector of length one), Default: numeric(0)

- min_1L_dbl:

  Minimum (a double vector of length one), Default: numeric(0)

- model_1L_int:

  Model (an integer vector of length one), Default: integer(0)

- plot_tfmn_fn:

  Plot transformation (a function), Default: identity

- summary_1L_lgl:

  Summary (a logical vector of length one), Default: FALSE

- tfmn_fn:

  Transformation (a function), Default: identity

- type_1L_chr:

  Type (a character vector of length one), Default: c("models",
  "candidates")

- uid_1L_chr:

  Unique identifier (a character vector of length one), Default: 'UID'

- use_1L_chr:

  Use (a character vector of length one), Default: character(0)

- var_1L_chr:

  Variable (a character vector of length one), Default: character(0)

- x_label_1L_chr:

  X label (a character vector of length one), Default: 'NA'

## Value

Regressions (a list)
