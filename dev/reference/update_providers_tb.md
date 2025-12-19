# Update providers tibble

update_providers_tb() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update providers tibble. The function returns
Providers (a tibble).

## Usage

``` r
update_providers_tb(providers_tb, var_1L_chr = "practitioner_category")
```

## Arguments

- providers_tb:

  Providers (a tibble)

- var_1L_chr:

  Variable (a character vector of length one), Default:
  'practitioner_category'

## Value

Providers (a tibble)
