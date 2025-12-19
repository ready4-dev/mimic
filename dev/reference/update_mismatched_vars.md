# Update mismatched variables

update_mismatched_vars() is an Update function that edits an object,
while preserving core object attributes. Specifically, this function
implements an algorithm to update mismatched variables. The function
returns Model dyad (a list).

## Usage

``` r
update_mismatched_vars(
  model_dyad_ls = make_model_dyad_ls,
  type_1L_chr = c("drop", "rename")
)
```

## Arguments

- model_dyad_ls:

  Model dyad (a list), Default: make_model_dyad_ls

- type_1L_chr:

  Type (a character vector of length one), Default: c("drop", "rename")

## Value

Model dyad (a list)
