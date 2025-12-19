# Make project comparison

make_project_cmprsn() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make project
comparison. The function returns Comparison (an output object of
multiple potential types).

## Usage

``` r
make_project_cmprsn(
  model_data_ls,
  by_1L_chr,
  include_chr,
  what_1L_chr = "X",
  digits_1L_int = 2L,
  labels_ls = NULL,
  tfmn_fn = identity,
  tfmn_1_fn = identity,
  tfmn_2_fn = identity,
  type_1L_chr = c("unimputed", "imputed", "combined")
)
```

## Arguments

- model_data_ls:

  Model data (a list)

- by_1L_chr:

  By (a character vector of length one)

- include_chr:

  Include (a character vector)

- what_1L_chr:

  What (a character vector of length one), Default: 'X'

- digits_1L_int:

  Digits (an integer vector of length one), Default: 2

- labels_ls:

  Labels (a list), Default: NULL

- tfmn_fn:

  Transformation (a function), Default: identity

- tfmn_1_fn:

  Transformation 1 (a function), Default: identity

- tfmn_2_fn:

  Transformation 2 (a function), Default: identity

- type_1L_chr:

  Type (a character vector of length one), Default: c("unimputed",
  "imputed", "combined")

## Value

Comparison (an output object of multiple potential types)
