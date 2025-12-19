# Add project 2 K10 draws

add_project_2_k10_draws() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add project 2 k10 draws. The function is
called for its side effects and does not return a value.

## Usage

``` r
add_project_2_k10_draws(
  X_Ready4useDyad,
  k10_severity_cuts_ls = make_k10_severity_cuts(),
  k10_var_1L_chr = "K10",
  scale_1L_dbl = 1,
  var_1L_chr = "K10",
  ...
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- k10_severity_cuts_ls:

  K10 severity cuts (a list), Default: make_k10_severity_cuts()

- k10_var_1L_chr:

  K10 variable (a character vector of length one), Default: 'K10'

- scale_1L_dbl:

  Scale (a double vector of length one), Default: 1

- var_1L_chr:

  Variable (a character vector of length one), Default: 'K10'

- ...:

  Additional arguments

## Value

X (A dataset and data dictionary pair.)
