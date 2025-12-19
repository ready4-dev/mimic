# Update order

update_order() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update order. The function is called for its
side effects and does not return a value.

## Usage

``` r
update_order(
  X_Ready4useDyad,
  structural_chr = make_structural_vars(data_1L_chr = character(0), uid_1L_chr = "UID"),
  type_1L_chr = c("rows", "columns")
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- structural_chr:

  Structural (a character vector), Default:
  make_structural_vars(data_1L_chr = character(0), uid_1L_chr = "UID")

- type_1L_chr:

  Type (a character vector of length one), Default: c("rows", "columns")

## Value

X (A dataset and data dictionary pair.)
