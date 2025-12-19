# Make utility functions list

make_utility_fns_ls() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make utility
functions list. The function returns Utility functions (a list).

## Usage

``` r
make_utility_fns_ls(
  add_to_ls = NULL,
  aqol8d_fn = add_aqol8d_from_k10,
  eq5d_fn = add_eq5d_from_draws,
  sf6d_fn = add_sf6d_from_draws,
  utilities_chr = c("AQoL8D", "EQ5D", "SF6D")
)
```

## Arguments

- add_to_ls:

  Add to (a list), Default: NULL

- aqol8d_fn:

  Assessment of Quality of Life Eight Dimension (a function), Default:
  add_aqol8d_from_k10

- eq5d_fn:

  EQ5D (a function), Default: add_eq5d_from_draws

- sf6d_fn:

  Short Form - Six Dimension (a function), Default: add_sf6d_from_draws

- utilities_chr:

  Utilities (a character vector), Default: c("AQoL8D", "EQ5D", "SF6D")

## Value

Utility functions (a list)
