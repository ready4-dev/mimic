# Make project consolidated dataset

make_project_consolidated_ds() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project consolidated dataset. The function is called for its side
effects and does not return a value.

## Usage

``` r
make_project_consolidated_ds(
  X_Ready4useDyad,
  Y_Ready4useDyad,
  Z_Ready4useDyad = ready4use::Ready4useDyad,
  type_1L_chr = c("outcomes", "tx_status")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- Y_Ready4useDyad:

  PARAM_DESCRIPTION

- Z_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad

- type_1L_chr:

  Type (a character vector of length one), Default: c("outcomes",
  "tx_status")

## Value

A (A dataset and data dictionary pair.)
